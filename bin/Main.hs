{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2017  Markus Ongyerth

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

Reach us at https://github.com/ongy/waymonad
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.IORef (readIORef)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.IO
import System.Environment (lookupEnv)

import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..))
import Graphics.Wayland.WlRoots.Render.Color (Color (..))
import Text.XkbCommon.InternalTypes (Keysym(..))
import Text.XkbCommon.KeysymList
import Text.XkbCommon.Keymap (noPrefs)

import Data.String (IsString)
import Fuse.Main
import Waymonad (Way, KeyBinding, getSeat)
import Waymonad.Actions.Spawn (spawn, manageSpawnOn)
import Waymonad.Actions.Spawn.X11 (manageX11SpawnOn)
import Waymonad.Actions.Startup.Environment
import Waymonad.GlobalFilter
import Waymonad.Hooks.EnterLeave (enterLeaveHook)
import Waymonad.Hooks.FocusFollowPointer
import Waymonad.Hooks.KeyboardFocus
import Waymonad.Hooks.ScaleHook
import Waymonad.IPC (IPCGroup (..))
import Waymonad.Input (attachDevice)
import Waymonad.Input.Cursor.Bindings (makeDefaultMappings)
import Waymonad.Input.Seat (useClipboardText, setSeatKeybinds, resetSeatKeymap)
import Waymonad.Layout (layoutOutput)
import Waymonad.Layout.AvoidStruts
import Waymonad.Layout.Choose
import Waymonad.Layout.Mirror (mkMirror, ToggleMirror (..))
import Waymonad.Layout.Ratio 
import Waymonad.Layout.SmartBorders
import Waymonad.Layout.Spiral 
import Waymonad.Layout.Tall 
import Waymonad.Layout.ToggleFull (mkTFull, ToggleFullM (..))
import Waymonad.Layout.TwoPane 
import Waymonad.Navigation2D
import Waymonad.Output (Output (outputRoots), addOutputToWork, setPreferdMode)
import Waymonad.Protocols.Background
import Waymonad.Protocols.DataControl
import Waymonad.Protocols.GammaControl
import Waymonad.Protocols.InputInhibit
import Waymonad.Protocols.LinuxDMABuf
import Waymonad.Protocols.DMAExport
import Waymonad.Protocols.Screenshooter
import Waymonad.Tabletv2
import Waymonad.Types (WayHooks (..), OutputEffective (..))
import Waymonad.Types.Core (WayKeyState, keystateAsInt, Seat (seatKeymap))
import Waymonad.Utility (sendMessage, focusNextOut, sendTo, closeCurrent, closeCompositor)
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Floating (centerFloat)
import Waymonad.Utility.Timing
import Waymonad.Utility.View
import Waymonad.Utility.ViewSet (modifyFocusedWS)
import Waymonad.ViewSet (WSTag, Layouted, FocusCore, ListLike (..))
import Waymonad.ViewSet.XMonad (ViewSet, sameLayout)

import Waymonad.IdleDPMS
import Waymonad.IdleManager
import Waymonad.Protocols.IdleInhibit

import qualified Data.IntMap.Strict as IM

import qualified Waymonad.Hooks.OutputAdd as H
import qualified Waymonad.Hooks.SeatMapping as SM
import qualified Waymonad.Shells.XWayland as XWay
import qualified Waymonad.Shells.XdgShell as Xdg
import qualified Waymonad.Shells.XdgShellv6 as Xdgv6
import qualified Waymonad.Shells.WlShell as Wl
import qualified Waymonad.Shells.Layers as Layer

import Waymonad.Main
import "waymonad" Config

import Graphics.Wayland.WlRoots.Util
import Waymonad.Output.DamageDisplay

wsSyms :: [Keysym]
wsSyms =
    [ keysym_1
    , keysym_2
    , keysym_3
    , keysym_4
    , keysym_5
    , keysym_6
    , keysym_7
    , keysym_8
    , keysym_9
    , keysym_0
    ]

workspaces :: IsString a => [a]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

makeListNavigation :: (ListLike vs ws, FocusCore vs ws, WSTag ws)
                   => Seat -> WlrModifier -> Way vs ws (WayKeyState -> Way vs ws Bool)
makeListNavigation seat modi = do
    current <- liftIO . readIORef $ seatKeymap seat
    let newMap = makeBindingMap
            [ (([modi], keysym_j), modifyFocusedWS $ flip _moveFocusRight)
            , (([modi], keysym_k), modifyFocusedWS $ flip _moveFocusLeft )
            , (([modi, Shift], keysym_h), resetSeatKeymap seat)
            , (([], keysym_Escape), resetSeatKeymap seat)
            ]

    pure $ \keystate -> case IM.lookup (keystateAsInt keystate) newMap of
        Nothing -> liftIO $ current keystate
        Just act -> act >> pure True

-- Use Logo as modifier when standalone, but Alt when started as child
getModi :: IO WlrModifier
getModi = do
    way <- lookupEnv "WAYLAND_DISPLAY"
    x11 <- lookupEnv "DISPLAY"
    pure . maybe Logo (const Alt) $ way <|> x11

printClipboard :: Way vs ws ()
printClipboard = doJust getSeat $ \seat ->
    useClipboardText seat $ liftIO . print

bindings :: (Layouted vs ws, ListLike vs ws, FocusCore vs ws, IsString ws, WSTag ws)
         => WlrModifier -> [(([WlrModifier], Keysym), KeyBinding vs ws)]
bindings modi =
    [ (([modi], keysym_k), moveUp)
    , (([modi], keysym_j), moveDown)
    , (([modi], keysym_h), moveLeft)
    , (([modi], keysym_l), moveRight)
    , (([modi, Shift], keysym_k), modifyFocusedWS $ flip _moveFocusedLeft )
    , (([modi, Shift], keysym_j), modifyFocusedWS $ flip _moveFocusedRight)
    , (([modi, Shift], keysym_h), doJust getSeat $ \seat -> do
            subMap <- makeListNavigation seat modi
            setSeatKeybinds seat subMap)
    , (([modi], keysym_f), sendMessage ToggleFullM)
    , (([modi], keysym_m), sendMessage ToggleMirror)
    , (([modi], keysym_space), sendMessage NextLayout)


    , (([modi], keysym_Left), sendMessage $ DecreaseRatio 0.1)
    , (([modi], keysym_Right), sendMessage $ IncreaseRatio 0.1)

    , (([modi], keysym_Return), spawn "weston-terminal")
    , (([modi], keysym_d), spawn "rofi -show run")

    , (([modi], keysym_o), centerFloat)
    , (([modi], keysym_p), printClipboard)

    , (([modi], keysym_n), focusNextOut)
    , (([modi], keysym_q), closeCurrent)
    , (([modi, Shift], keysym_e), closeCompositor)
    ] ++ concatMap (\(sym, ws) -> [(([modi], sym), greedyView ws), (([modi, Shift], sym), sendTo ws), (([modi, Ctrl], sym), copyView ws)]) (zip wsSyms workspaces)

myConf :: WlrModifier -> WayUserConf (ViewSet Text) Text
myConf modi = WayUserConf
    { wayUserConfWorkspaces   = workspaces
    , wayUserConfLayouts      = sameLayout . avoidStruts . mkSmartBorders 2. mkTFull . mkMirror $ (Tall 0.5 ||| TwoPane 0.5 ||| Spiral 0.618)
    , wayUserConfManagehook   = XWay.overrideXRedirect <> manageSpawnOn <> manageX11SpawnOn
    , wayUserConfEventHook    = idleDPMSHandler <> idleLog
    , wayUserConfKeybinds     = bindings modi
    , wayUserConfPointerbinds = makeDefaultMappings modi

    , wayUserConfInputAdd    = flip attachDevice "seat0"
    , wayUserConfDisplayHook =
        [ getFuseBracket (IPCGroup  [("IdleManager", Right $ idleIPC (Proxy :: Proxy IdleEvent))])
        , getGammaBracket
        , getDataControlBracket
        , getFilterBracket filterUser
        , baseTimeBracket
        , envBracket [ ("QT_QPA_PLATFORM", "wayland-egl")
                     -- breaks firefox (on arch) :/
                     --, ("GDK_BACKEND", "wayland")
                     , ("SDL_VIDEODRIVER", "wayland")
                     , ("CLUTTER_BACKEND", "wayland")
                     ]
        , getIdleInihibitBracket
        , getBackgroundBracket
        , getInputInhibitBracket
        , getDMAExporterBracket
        , getTabletv2Bracket
        ]
    , wayUserConfBackendHook = [getIdleBracket 6e5 {- 10 minutes in ms -}, getLinuxDMABufBracket]
    , wayUserConfPostHook    = [getScreenshooterBracket]
    , wayUserConfCoreHooks   = WayHooks
        { wayHooksVWSChange       = wsScaleHook
        , wayHooksOutputMapping   = enterLeaveHook <> handlePointerSwitch <> SM.mappingChangeEvt <> managedStrutHandler
        , wayHooksSeatWSChange    = SM.wsChangeLogHook <> handleKeyboardSwitch
        , wayHooksSeatOutput      = SM.outputChangeEvt {-<> handleKeyboardPull-} <> handlePointerPull
        , wayHooksSeatFocusChange = focusFollowPointer
        , wayHooksNewOutput       = H.outputAddHook
        , wayHooksOutputEffective = layoutOutput . getChangedOutput <> const Layer.forceLayout
        }
    , wayUserConfShells = [Xdg.makeShell, Xdgv6.makeShell, Wl.makeShell, XWay.makeShell, Layer.makeShell]
    , wayUserConfLog = pure ()
    , wayUserConfOutputAdd = \out -> setPreferdMode (outputRoots out) $
        addOutputToWork out Nothing
    , wayUserconfLoggers = Nothing
    , wayUserconfColor = Color 0.5 0 0 1
    , wayUserconfColors = mempty
    , wayUserconfFramerHandler = Nothing -- Just $ damageDisplay 30
    , wayUserconfXKBMap = const noPrefs
    }

main :: IO ()
main = do
    setLogPrio Debug
    modi <- getModi
    confE <- loadConfig
    case confE of
        Left err -> do
            hPutStrLn stderr "Couldn't load config:"
            hPutStrLn stderr err
            printConfigInfo
        Right conf -> wayUserMain $ modifyConfig conf (myConf modi)
