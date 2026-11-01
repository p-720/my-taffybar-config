{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (finally)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (foldM, forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.GI.Base (unsafeCastTo)
import GHC.IO.Encoding (setLocaleEncoding)
import Data.Unique (Unique, newUnique)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified StatusNotifier.Tray as Tray
import System.Exit (ExitCode (..))
import System.IO (hClose, hIsEOF, hSetEncoding, utf8)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM, signalProcessGroup)
import System.Process
import System.Taffybar (dyreTaffybar)
import System.Taffybar.Context (Context (..), TaffyIO)
import System.Taffybar.Information.Audio (AudioInfo (..), defaultAudioPulseSink, defaultAudioWirePlumberNode, getAudioInfoChanAndVar)
import System.Taffybar.Information.EWMHDesktopInfo (getWorkspaceNames, switchOneWorkspace, withX11Context)
import System.Taffybar.Information.Memory (MemoryInfo (..), parseMeminfo)
import System.Taffybar.Information.X11DesktopInfo (DisplayName (..))
import System.Taffybar.SimpleConfig
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget
import System.Taffybar.Information.Workspaces.Model (WorkspaceInfo(..), WorkspaceIdentity(..))
import System.Taffybar.Widget.Audio (audioNewWith, defaultAudioWidgetConfig)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import System.Taffybar.Widget.WttrIn (textWttrNew)
import Text.Printf (printf)

main :: IO ()
main = do
  setLocaleEncoding utf8
  installTerminationHandler
  dyreTaffybar
    ( toTaffybarConfigPerMonitor baseBarConfig $ \monitorIndex -> do
        primary <- isPrimaryMonitor monitorIndex
        pure $
          SimpleMonitorConfig
            (if primary then "primary" else "secondary")
            (if primary then primaryBarConfig else secondaryBarConfig)
    )
    `finally` terminateAllProcesses

baseBarConfig :: SimpleTaffyConfig
baseBarConfig =
  defaultSimpleTaffyConfig
    { barHeight = ExactSize 30,
      barPadding = 0,
      barPosition = Bottom,
      widgetSpacing = 8,
      monitorsAction = useAllMonitors,
      startupHook = attachBarScroll
    }

primaryBarConfig :: SimpleTaffyConfig
primaryBarConfig =
  baseBarConfig
    { barCssClasses = ["primary"],
      startWidgets = [workspacesWidget Nothing, inhibitorWidget, memoryWidget],
      centerWidgets = [gcalWidget, clockWidget],
      endWidgets = [audioWidget, weatherWidget, polypomoWidget, pomotaskWidget]
    }

secondaryBarConfig :: SimpleTaffyConfig
secondaryBarConfig =
  baseBarConfig
    { barCssClasses = ["secondary"],
      startWidgets = [workspacesWidget (Just (\ws -> maybe False (\n -> n >= 6 && n < 12) (workspaceNumericId (workspaceIdentity ws)))), inhibitorWidget, memoryWidget, diskWidget, pomotaskWidget],
      centerWidgets = [gcalWidget, clockWidget],
      endWidgets = [trayWidget, audioWidget]
    }

isPrimaryMonitor :: Int -> TaffyIO Bool
isPrimaryMonitor monitorIndex = liftIO $ do
  maybeDisplay <- Gdk.displayGetDefault
  case maybeDisplay of
    Nothing -> pure (monitorIndex == 0)
    Just display -> do
      maybePrimary <- Gdk.displayGetPrimaryMonitor display
      maybeMonitor <- Gdk.displayGetMonitor display (fromIntegral monitorIndex)
      pure $ case (maybePrimary, maybeMonitor) of
        (Just primary, Just monitor) -> primary == monitor
        _ -> monitorIndex == 0

workspacesWidget :: Maybe (WorkspaceInfo -> Bool) -> TaffyIO Gtk.Widget
workspacesWidget visibleOverride = do
  visible <- case visibleOverride of
    Just f -> pure f
    Nothing -> do
      monitors <- liftIO $ do
        maybeDisplay <- Gdk.displayGetDefault
        maybe (pure 1) Gdk.displayGetNMonitors maybeDisplay
      pure $ if monitors > 1
        then \ws -> maybe False (< 6) (workspaceNumericId (workspaceIdentity ws))
        else \ws -> maybe False (< 12) (workspaceNumericId (workspaceIdentity ws))
  widget <-
    workspacesNew
      defaultWorkspacesConfig
        { showWorkspaceFn = visible,
          urgentWorkspaceState = True,
          iconSize = Just 18,
          maxIcons = Just 4,
          widgetGap = 4
        }
  -- the icon strip can outgrow its allocation while app icons load; the
  -- margin keeps the next module out of the overflow instead of under it
  liftIO $ do
    Gtk.widgetSetMarginEnd widget 16
    addWorkspaceScroll widget
  pure widget

inhibitorWidget :: TaffyIO Gtk.Widget
inhibitorWidget = do
  box <- liftIO Gtk.eventBoxNew
  _ <- widgetSetClassGI box "module"
  _ <- widgetSetClassGI box "idle_inhibitor"
  liftIO $ addWorkspaceScroll box
  label <- liftIO $ Gtk.labelNew (Just (T.pack "\xF070"))
  inner <- liftIO $ padded label
  liftIO $ do
    Gtk.containerAdd box inner
    -- single fixed-size glyph: reserve the waybar block so the eye is centred
    -- instead of squeezed into the label's own width
    Gtk.widgetSetSizeRequest box 40 (-1)
    _ <- addClassIfMissing "idle-inhibitor-inactive" box
    Gtk.widgetShowAll box
  stateVar <- liftIO $ newMVar Nothing
  _ <- liftIO $
    Gtk.onWidgetButtonPressEvent box $ \event ->
      catchAny
        ( do
            button <- Gdk.getEventButtonButton event
            when (button == 1) $
              modifyMVar_ stateVar $ \current ->
                case current of
                  Nothing -> do
                    (_, _, _, processHandle) <-
                      createProcess
                        ( proc
                            "/run/current-system/sw/bin/systemd-inhibit"
                            [ "--what=idle",
                              "--who=taffybar",
                              "--why=Idle inhibitor toggled from the bar",
                              "--mode=block",
                              "sleep",
                              "infinity"
                            ]
                        )
                          { create_group = True
                          }
                    key <- registerProcess processHandle
                    postGUIASync $ do
                      Gtk.labelSetText label (T.pack "\xF06E")
                      _ <- addClassIfMissing "idle-inhibitor-active" box
                      removeClassIfPresent "idle-inhibitor-inactive" box
                      Gtk.widgetQueueResize box
                    pure (Just (key, processHandle))
                  Just (key, processHandle) -> do
                    catchAny
                      (killProcessGroup processHandle >> void (waitForProcess processHandle))
                      (\_ -> pure ())
                    forgetProcess key
                    postGUIASync $ do
                      Gtk.labelSetText label (T.pack "\xF070")
                      _ <- addClassIfMissing "idle-inhibitor-inactive" box
                      removeClassIfPresent "idle-inhibitor-active" box
                      Gtk.widgetQueueResize box
                    pure Nothing
            pure True
        )
        (\_ -> pure True)
  Gtk.toWidget box

memoryWidget :: TaffyIO Gtk.Widget
memoryWidget = do
  widget <-
    pollingLabelNew 2 $ do
      info <- parseMeminfo
      let gib :: Double -> String
          gib value = printf "%.1f" (value / 1024)
      pure $
        T.pack $
          printf
            "\xF035B %s/%s \xF04E2 %s/%s"
            (gib (memoryUsed info))
            (gib (memoryTotal info))
            (gib (memorySwapUsed info))
            (gib (memorySwapTotal info))
  moduleWrap "memory" widget

diskWidget :: TaffyIO Gtk.Widget
diskWidget =
  diskUsageLabelNewWith
    defaultDiskUsageWidgetConfig
      { diskUsageFormat = "\xF0C7 $usedPercent$%"
      }
    >>= moduleWrap "disk"

audioWidget :: TaffyIO Gtk.Widget
audioWidget = do
  widget <- audioNewWith defaultAudioWidgetConfig >>= moduleWrap "pulseaudio"
  (chan, stateVar) <- getAudioInfoChanAndVar defaultAudioPulseSink defaultAudioWirePlumberNode
  let updateMuted maybeInfo = postGUIASync $
        case maybeInfo >>= audioMuted of
          Just True -> addClassIfMissing "muted" widget
          _ -> removeClassIfPresent "muted" widget
  liftIO $ readMVar stateVar >>= updateMuted
  _ <- channelWidgetNew widget chan updateMuted
  pure widget

clockWidget :: TaffyIO Gtk.Widget
clockWidget =
  textClockNewWith
    defaultClockConfig
      { clockFormatString = "%a, %d %b %H:%M"
      }
    >>= moduleWrap "clock"

gcalWidget :: TaffyIO Gtk.Widget
gcalWidget =
  commandRunnerNew
    300
    "bash"
    ["-c", "/etc/nixos/configs/waybar/taffybar_gcal.sh"]
    "No current event"
    >>= moduleWrap "gcal"

weatherWidget :: TaffyIO Gtk.Widget
weatherWidget =
  textWttrNew "https://wttr.in/Moscow?format=%c%t%20(%f)" 600
    >>= moduleWrap "weather"

pomotaskWidget :: TaffyIO Gtk.Widget
pomotaskWidget =
  streamingCommand
    "pomotask"
    -- setpriv pdeathsig: pkill taffybar reparents children instead of reaping
    -- them, so a plain child leaks a timer process on every bar restart
    "exec setpriv --pdeathsig TERM /etc/nixos/configs/waybar/polybar-pomotask.py"
    [(1, "/etc/nixos/configs/waybar/polybar-pomotask.py --stop")]
    Nothing

polypomoWidget :: TaffyIO Gtk.Widget
polypomoWidget =
  streamingCommand
    "polypomo"
    "exec setpriv --pdeathsig TERM /home/andrew/.config/polybar/polypomo --saveto /etc/nixos/configs/polybar/time.sqlite"
    [ (1, "~/.config/polybar/polypomo toggle"),
      (2, "~/.config/polybar/polypomo time -1410"),
      (3, "~/.config/polybar/polypomo end")
    ]
    Nothing

trayWidget :: TaffyIO Gtk.Widget
trayWidget = do
  widget <-
    sniTrayNewFromParams
      Tray.defaultTrayParams
        { Tray.trayImageSize = Tray.TrayImageSize 18
        }
  -- the tray box is built with spacing 0 and exposes no spacing knob in
  -- TrayParams; cast back to its real Box type and set it there
  liftIO $ do
    trayBox <- unsafeCastTo Gtk.Box widget
    Gtk.boxSetSpacing trayBox 10
  moduleWrap "tray" widget

data TrackedProcess = TrackedProcess
  { trackedKey :: Unique,
    trackedHandle :: ProcessHandle
  }

{-# NOINLINE processGroups #-}
processGroups :: MVar [TrackedProcess]
processGroups = unsafePerformIO (newMVar [])

registerProcess :: ProcessHandle -> IO Unique
registerProcess handle = do
  key <- newUnique
  modifyMVar_ processGroups (pure . (TrackedProcess key handle :))
  pure key

forgetProcess :: Unique -> IO ()
forgetProcess key =
  modifyMVar_ processGroups (pure . filter ((/= key) . trackedKey))

killProcessGroup :: ProcessHandle -> IO ()
killProcessGroup handle = do
  maybePid <- getPid handle
  case maybePid of
    Just pid -> signalProcessGroup sigTERM pid
    Nothing -> pure ()

terminateAllProcesses :: IO ()
terminateAllProcesses =
  withMVar processGroups (mapM_ (killProcessGroup . trackedHandle))

-- pkill taffybar sends SIGTERM, which GTK never sees, so the stream children
-- are reparented instead of reaped; kill their process groups explicitly.
installTerminationHandler :: IO ()
installTerminationHandler =
  void $
    installHandler
      sigTERM
      (Catch (terminateAllProcesses >> exitImmediately ExitSuccess))
      Nothing

attachBarScroll :: TaffyIO ()
attachBarScroll = do
  context <- ask
  liftIO $ void $ forkIO $ loop (existingWindows context) []
  where
    loop windowsVar attached = do
      windows <- readMVar windowsVar
      attached' <-
        foldM
          ( \acc (_, window) ->
              if window `elem` acc
                then pure acc
                else do
                  -- GTK calls must happen on the GUI thread, not this poller
                  postGUIASync $ addWorkspaceScroll window
                  pure (window : acc)
          )
          attached
          windows
      threadDelay 1000000
      loop windowsVar attached'

addWorkspaceScroll :: Gtk.IsWidget widget => widget -> IO ()
addWorkspaceScroll widget = do
  Gtk.widgetAddEvents widget [Gdk.EventMaskScrollMask]
  void $
    Gtk.onWidgetScrollEvent widget $ \event -> do
      direction <- Gdk.getEventScrollDirection event
      case direction of
        Gdk.ScrollDirectionUp -> switchWorkspace True
        Gdk.ScrollDirectionLeft -> switchWorkspace True
        Gdk.ScrollDirectionDown -> switchWorkspace False
        Gdk.ScrollDirectionRight -> switchWorkspace False
        _ -> pure False
  where
    switchWorkspace previous =
      catchAny
        ( do
            withX11Context DefaultDisplay $ do
              names <- getWorkspaceNames
              switchOneWorkspace previous (max 0 (length names - 1))
            pure True
        )
        (\_ -> pure True)

padded :: Gtk.IsWidget widget => widget -> IO Gtk.Widget
padded child = do
  alignment <- Gtk.alignmentNew 0.5 0.5 1 1
  Gtk.alignmentSetPadding alignment 0 0 12 12
  Gtk.containerAdd alignment child
  Gtk.widgetShowAll alignment
  Gtk.toWidget alignment

moduleWrap :: T.Text -> Gtk.Widget -> TaffyIO Gtk.Widget
moduleWrap klass child = do
  box <- liftIO Gtk.eventBoxNew
  _ <- widgetSetClassGI box "module"
  _ <- widgetSetClassGI box klass
  liftIO $ addWorkspaceScroll box
  inner <- liftIO $ padded child
  liftIO $ do
    Gtk.containerAdd box inner
    Gtk.widgetShowAll box
  Gtk.toWidget box

streamingCommand ::
  T.Text ->
  String ->
  [(Int, String)] ->
  Maybe (String, String) ->
  TaffyIO Gtk.Widget
streamingCommand klass command buttons scrollActions = do
  box <- liftIO Gtk.eventBoxNew
  _ <- widgetSetClassGI box "module"
  _ <- widgetSetClassGI box klass
  liftIO $ addWorkspaceScroll box
  label <- liftIO $ Gtk.labelNew Nothing
  inner <- liftIO $ padded label
  liftIO $ do
    -- width-chars keeps the empty module from requisitioning zero width, but
    -- unlike a size request it still lets the first stream line grow the box
    Gtk.labelSetWidthChars label 2
    Gtk.containerAdd box inner
    Gtk.widgetShowAll box
    -- an idle pomodoro has no text; leaving its block visible is just an
    -- empty coloured square
    Gtk.widgetSetVisible box False
  _ <- liftIO $
    Gtk.onWidgetButtonPressEvent box $ \event -> do
      button <- Gdk.getEventButtonButton event
      forM_ buttons $ \(wanted, action) ->
        when (fromIntegral button == wanted) $ void $ spawnCommand action
      pure True
  liftIO $
    forM_ scrollActions $ \(upAction, downAction) ->
      void $
        Gtk.onWidgetScrollEvent box $ \event -> do
          direction <- Gdk.getEventScrollDirection event
          case direction of
            Gdk.ScrollDirectionUp -> spawnCommand upAction >> pure True
            Gdk.ScrollDirectionDown -> spawnCommand downAction >> pure True
            _ -> pure False
  _ <- liftIO $
    forkIO $
      forever $ do
        catchAny (runStream box label) (\_ -> pure ())
        threadDelay 2000000
  Gtk.toWidget box
  where
    runStream box label = do
      (_, maybeOut, _, processHandle) <-
        createProcess (proc "bash" ["-c", command]) {std_out = CreatePipe, std_err = NoStream, create_group = True}
      key <- registerProcess processHandle
      let runChild =
            case maybeOut of
              Nothing -> pure ()
              Just out -> do
                hSetEncoding out utf8
                let loop = do
                      endOfFile <- hIsEOF out
                      unless endOfFile $ do
                        line <- TIO.hGetLine out
                        postGUIASync $ do
                          let text = T.dropWhileEnd (== '\r') line
                          Gtk.labelSetText label text
                          Gtk.widgetSetVisible box (not (T.null (T.strip text)))
                          Gtk.widgetQueueResize box
                        loop
                catchAny loop (\_ -> void $ terminateProcess processHandle)
                hClose out
      runChild `finally` forgetProcess key
      void $ waitForProcess processHandle
