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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE FlexibleContexts #-}
module Main
where

import Control.Applicative ((<|>))
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import System.IO
import System.Environment (lookupEnv)

import Text.XkbCommon.InternalTypes (Keysym(..))
import Text.XkbCommon.KeysymList
import Graphics.Wayland.WlRoots.Backend.Libinput (getDeviceHandle)
import Graphics.Wayland.WlRoots.Input (InputDevice, getDeviceName)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..))

import Data.String (IsString)
import Fuse.Main
import GlobalFilter
import Hooks.EnterLeave (enterLeaveHook)
import Hooks.FocusFollowPointer
import Hooks.KeyboardFocus
import Hooks.ScaleHook
import IdleManager
import Input (attachDevice)
import Layout.Choose
import Layout.Mirror (mkMirror, ToggleMirror (..))
import Layout.Spiral
import Layout.Tall (Tall (..))
import Layout.ToggleFull (mkTFull, ToggleFullM (..))
import Layout.TwoPane (TwoPane (..))
import Navigation2D
import Protocols.GammaControl
import Protocols.Screenshooter
import Startup.Environment
import Startup.Generic
import Utility (doJust)
import Utility.Spawn (spawn, manageSpawnOn)
import View.Proxy (makeProxy)
import ViewSet (WSTag, Layouted, FocusCore, ListLike (..))
import WayUtil (sendMessage, focusNextOut, sendTo, killCurrent, closeCompositor)
import WayUtil.Current (getCurrentView)
import WayUtil.Floating (centerFloat)
import WayUtil.Timing
import WayUtil.View
import WayUtil.ViewSet (modifyFocusedWS)
import Waymonad (Way, KeyBinding)
import Waymonad.Types (SomeEvent, WayHooks (..))
import XMonad.ViewSet (ViewSet, sameLayout)

import qualified Hooks.OutputAdd as H
import qualified Hooks.SeatMapping as SM
import qualified Shells.XWayland as XWay
import qualified Shells.XdgShell as Xdg
import qualified System.InputDevice as LI
import qualified View.Multi as Multi
import qualified Data.Text as T

import Waymonad.Main
import Config

import Graphics.Wayland.WlRoots.Util

setupTrackball :: Ptr InputDevice -> IO ()
setupTrackball dev = doJust (getDeviceHandle dev) $ \handle -> do
    name <- getDeviceName dev
    when ("Logitech USB Trackball" `T.isPrefixOf` name) $ do
        void $ LI.setScrollMethod handle LI.ScrollOnButtonDown
        void $ LI.setScrollButton handle 0x116

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
    , keysym_v
    ]

workspaces :: IsString a => [a]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "video"]

-- Use Logo as modifier when standalone, but Alt when started as child
getModi :: IO WlrModifier
getModi = do
    way <- lookupEnv "WAYLAND_DISPLAY"
    x11 <- lookupEnv "DISPLAY"
    pure . maybe Logo (const Alt) $ way <|> x11

bindings :: (Layouted vs ws, ListLike vs ws, FocusCore vs ws, IsString ws, WSTag ws)
         => WlrModifier -> [(([WlrModifier], Keysym), KeyBinding vs ws)]
bindings modi =
    [ (([modi], keysym_k), moveUp)
    , (([modi], keysym_j), moveDown)
    , (([modi], keysym_h), moveLeft)
    , (([modi], keysym_l), moveRight)
    , (([modi, Shift], keysym_k), modifyFocusedWS $ flip _moveFocusedLeft )
    , (([modi, Shift], keysym_j), modifyFocusedWS $ flip _moveFocusedRight)
    , (([modi], keysym_f), sendMessage ToggleFullM)
    , (([modi], keysym_m), sendMessage ToggleMirror)
    , (([modi], keysym_space), sendMessage NextLayout)

    , (([modi], keysym_Return), spawn "alacritty")
    , (([modi], keysym_d), spawn "dmenu_run")
    , (([modi], keysym_w), spawn "vwatch")
    , (([modi], keysym_t), spawn "mpc toggle")
    , (([modi, Shift], keysym_leftarrow), spawn "mpc prev")
    , (([modi, Shift], keysym_rightarrow), spawn "mpc next")

    , (([modi], keysym_n), focusNextOut)
    , (([modi], keysym_q), killCurrent)
    , (([modi], keysym_o), centerFloat)
    , (([modi], keysym_c), doJust getCurrentView makeProxy)
    , (([modi], keysym_a), doJust getCurrentView Multi.copyView)
    , (([modi, Shift], keysym_e), closeCompositor)
    ] ++ concatMap (\(sym, ws) -> [(([modi], sym), greedyView ws), (([modi, Shift], sym), sendTo ws)]) (zip wsSyms workspaces)

myEventHook :: (FocusCore vs a, WSTag a) => SomeEvent -> Way vs a ()
myEventHook = idleLog

myConf :: WlrModifier -> WayUserConf (ViewSet Text) Text
myConf modi = WayUserConf
    { wayUserConfWorkspaces  = workspaces
    , wayUserConfLayouts     = sameLayout .  mkMirror . mkTFull $ (Tall ||| TwoPane ||| Spiral)
    , wayUserConfManagehook  = XWay.overrideXRedirect <> manageSpawnOn
    , wayUserConfEventHook   = myEventHook
    , wayUserConfKeybinds    = bindings modi

    , wayUserConfInputAdd    = \ptr -> do
        liftIO $ setupTrackball ptr
        attachDevice ptr "seat0"
    , wayUserConfDisplayHook = [getFuseBracket, getGammaBracket, getFilterBracket filterUser, baseTimeBracket, getStartupBracket (spawn "redshift -m wayland"), envBracket [("PULSE_SERVER", "zelda.ongy")]]
    , wayUserConfBackendHook = [getIdleBracket 3e5]
    , wayUserConfPostHook    = [getScreenshooterBracket]
    , wayUserConfCoreHooks   = WayHooks
        { wayHooksVWSChange     = wsScaleHook <> (liftIO . hPrint stderr)
        , wayHooksOutputMapping = enterLeaveHook <> handlePointerSwitch <> SM.mappingChangeEvt <> (liftIO . hPrint stderr)
          , wayHooksSeatWSChange  = SM.wsChangeLogHook <> handleKeyboardSwitch <> (liftIO . hPrint stderr)
        , wayHooksSeatOutput = SM.outputChangeEvt <> (liftIO . hPrint stderr)
        , wayHooksSeatFocusChange = focusFollowPointer <> (liftIO . hPrint stderr)
        }
    , wayUserConfShells = [Xdg.makeShell, XWay.makeShell]
    , wayUserConfLog = pure ()
    , wayUserConfOutputAdd = H.outputAddHook
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
        Right conf -> wayUserMain $ modifyConfig conf (myConf modi)
