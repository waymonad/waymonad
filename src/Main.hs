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

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.IO

import Text.XkbCommon.InternalTypes (Keysym(..))
import Text.XkbCommon.KeysymList
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..))

import Data.String (IsString)
import Fuse.Main
import GlobalFilter
import Hooks.EnterLeave (enterLeaveHook)
import Hooks.FocusFollowPointer
import Hooks.KeyboardFocus
import Hooks.ScaleHook
import IdleManager
import InjectRunner
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

import qualified View.Multi as Multi
import qualified Hooks.OutputAdd as H
import qualified Hooks.SeatMapping as SM
import qualified Shells.XdgShell as Xdg
import qualified Shells.XWayland as XWay

import Waymonad.Main


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

bindings
    :: (Layouted vs ws, ListLike vs ws, FocusCore vs ws, IsString ws, WSTag ws)
    => [(([WlrModifier], Keysym), KeyBinding vs ws)]
bindings =
    [ (([modi], keysym_k), moveUp)
    , (([modi], keysym_j), moveDown)
    , (([modi], keysym_h), moveLeft)
    , (([modi], keysym_l), moveRight)
    , (([modi, Shift], keysym_k), modifyFocusedWS $ flip _moveFocusedLeft )
    , (([modi, Shift], keysym_j), modifyFocusedWS $ flip _moveFocusedRight)
    , (([modi], keysym_f), sendMessage ToggleFullM)
    , (([modi], keysym_m), sendMessage ToggleMirror)
    , (([modi], keysym_Right), sendMessage NextLayout)

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
    where modi = Alt

myEventHook :: (FocusCore vs a, WSTag a) => SomeEvent -> Way vs a ()
myEventHook = H.outputAddHook <> idleLog

myConf :: WayUserConf (ViewSet Text) Text
myConf = WayUserConf
    { wayUserConfWorkspaces  = workspaces
    , wayUserConfLayouts     = sameLayout .  mkMirror . mkTFull $ (Tall ||| TwoPane ||| Spiral)
    , wayUserConfManagehook  = XWay.overrideXRedirect <> manageSpawnOn
    , wayUserConfEventHook   = myEventHook
    , wayUserConfKeybinds    = bindings

    , wayUserConfDisplayHook = [getFuseBracket, getGammaBracket, getFilterBracket filterUser, injectBracket, baseTimeBracket]
    , wayUserConfBackendHook = [getIdleBracket 3e5]
    , wayUserConfPostHook    = [getScreenshooterBracket, envBracket [("PULSE_SERVER", "zelda.ongy")]]
    , wayUserConfCoreHooks   = WayHooks
        { wayHooksVWSChange     = wsScaleHook <> (liftIO . hPrint stderr)
        , wayHooksOutputMapping = enterLeaveHook <> SM.mappingChangeEvt <> (liftIO . hPrint stderr)
          , wayHooksSeatWSChange  = SM.wsChangeLogHook <> handleKeyboardSwitch <> (liftIO . hPrint stderr)
        , wayHooksSeatOutput = SM.outputChangeEvt <> (liftIO . hPrint stderr)
        , wayHooksSeatFocusChange = focusFollowPointer <> (liftIO . hPrint stderr)
        }
    , wayUserConfShells = [Xdg.makeShell, XWay.makeShell]
    , wayUserConfLog = pure ()
    }

main :: IO ()
main = wayUserMain myConf
