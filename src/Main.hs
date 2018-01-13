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

import Layout.Quadrant

import Startup.Generic
import Startup.Environment
import Data.String (IsString)
import Protocols.Screenshooter
import Protocols.GammaControl
import GlobalFilter
import IdleManager
import InjectRunner
--import System.Posix.Signals
import Fuse.Main
import Hooks.EnterLeave (enterLeaveHook)
import Layout.Spiral
import Layout.Choose

import qualified Hooks.OutputAdd as H
import WayUtil.View
import WayUtil.Timing
import qualified Hooks.SeatMapping as SM
import Hooks.KeyboardFocus
import Hooks.ScaleHook
import Log

import Config

-- import Control.Concurrent (runInBoundThread)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, IORef, writeIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.Server (DisplayServer, displayInitShm)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import Text.XkbCommon.InternalTypes (Keysym(..))
import Text.XkbCommon.KeysymList

import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Compositor (compositorCreate)
import Graphics.Wayland.WlRoots.DeviceManager (managerCreate)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..), modifiersToField)
import Graphics.Wayland.WlRoots.OutputLayout (createOutputLayout)
import Graphics.Wayland.WlRoots.Render.Gles2 (rendererCreate)

import Input (inputCreate)
import Layout.Mirror (mkMirror, ToggleMirror (..))
import Layout.Tall (Tall (..))
import Layout.ToggleFull (mkTFull, ToggleFullM (..))
import Output (handleOutputAdd, handleOutputRemove)
import Shared (CompHooks (..), ignoreHooks, launchCompositor, Bracketed (..))
import Utility (doJust)
import Utility.Spawn (spawn, manageSpawnOn, spawnOn)
import qualified View.Multi as Multi
import View.Proxy (makeProxy)
import ViewSet
    ( GenericLayout (..)
    , WSTag
    , Layouted
    , FocusCore
    , ListLike (..)
    , GenericLayoutClass
    )
import Waymonad
    ( Way
    , runWay
    , BindingMap
    , KeyBinding
    , WayBindingState (..)
    , makeCallback

    , WayLoggers (..)
    , Logger (..)
    )
import Waymonad.Types (Compositor (..), LogPriority (..), Managehook, SomeEvent, WayHooks (..), WayShell)
import WayUtil
    ( sendMessage
    , focusNextOut
    , sendTo
    , killCurrent
    , seatOutputEventLogger
    , closeCompositor
    )
import WayUtil.Current (getCurrentView)
import WayUtil.ViewSet (modifyFocusedWS)
import WayUtil.Floating (centerFloat)
import XMonad.ViewSet (ViewSet, Workspace (..))

import qualified Data.Map.Strict as M
import qualified Shells.XdgShell as Xdg
import qualified Shells.XWayland as XWay


makeBindingMap :: [(([WlrModifier], Keysym), KeyBinding vs a)] -> BindingMap vs a
makeBindingMap = M.fromList .
    map (\((mods, Keysym sym), fun) -> ((modifiersToField mods, sym), fun))

makeCompositor
    :: (FocusCore vs a, WSTag a)
    => DisplayServer
    -> Ptr Backend
    -> Way vs a Compositor
makeCompositor display backend = do
    liftIO $ hPutStrLn stderr "Creating compositor"
    renderer <- liftIO $ rendererCreate backend
    void $ liftIO $ displayInitShm display
    comp <- liftIO $ compositorCreate display renderer
    devManager <- liftIO $ managerCreate display
    layout <- liftIO createOutputLayout

    input <- inputCreate backend

    pure Compositor
        { compDisplay = display
        , compRenderer = renderer
        , compCompositor = comp
        , compManager = devManager
        , compBackend = backend
        , compLayout = layout
        , compInput = input
        }

sameLayout
    :: (WSTag a, GenericLayoutClass l (ViewSet a) a)
    => l -> [a] -> M.Map a (Workspace a)
sameLayout l = M.fromList . map (, Workspace (GenericLayout (l)) Nothing)

data WayUserConf vs ws = WayUserConf
    { wayUserConfWorkspaces  :: [ws]
    , wayUserConfLayouts     :: [ws] -> vs
    , wayUserConfManagehook  :: Managehook vs ws
    , wayUserConfEventHook   :: SomeEvent -> Way vs ws ()
    , wayUserConfKeybinds    :: [(([WlrModifier], Keysym), KeyBinding vs ws)]

    , wayUserConfDisplayHook :: [Bracketed vs DisplayServer ws]
    , wayUserConfBackendHook :: [Bracketed vs (DisplayServer, Ptr Backend) ws]
    , wayUserConfPostHook    :: [Bracketed vs () ws]
    , wayUserConfCoreHooks   :: WayHooks vs ws
    , wayUserConfShells      :: [IO WayShell]
    }

wayUserRealMain :: (FocusCore vs a, WSTag a) => WayUserConf vs a -> IORef Compositor -> Way vs a ()
wayUserRealMain conf compRef = do
    setBaseTime

    outputAdd <- makeCallback $ handleOutputAdd compRef $ wayUserConfWorkspaces conf
    outputRm  <- makeCallback handleOutputRemove

    compFun <- pure $ \(display, backend) -> liftIO . writeIORef compRef =<<  makeCompositor display backend

    launchCompositor ignoreHooks
        { displayHook =  wayUserConfDisplayHook conf
        , backendPreHook = Bracketed compFun (const $ pure ()): wayUserConfBackendHook conf
        , backendPostHook  = wayUserConfPostHook conf

        , outputAddHook    = outputAdd
        , outputRemoveHook = outputRm
        }

wayUserMain :: (FocusCore vs a, WSTag a) => WayUserConf vs a -> IO ()
wayUserMain conf = do
    configE <- loadConfig
    case configE of
        Left str -> do
            -- TODO: This should probably be visual later on when possible
            hPutStrLn stderr "Error while loading config:"
            hPutStrLn stderr str
        Right config -> do
            stateRef  <- newIORef $ wayUserConfLayouts conf $ wayUserConfWorkspaces conf
            layoutRef <- newIORef mempty
            mapRef <- newIORef []
            currentRef <- newIORef []
            outputs <- newIORef []
            seats <- newIORef []
            extensible <- newIORef mempty
            floats <- newIORef mempty
            compRef <- newIORef $ error "Tried to access compositor to early"
            logF <- logFun
            inject <- makeInject
            shells <- sequence $ wayUserConfShells conf

            let state = WayBindingState
                    { wayBindingCache = layoutRef
                    , wayBindingState = stateRef
                    , wayBindingCurrent = currentRef
                    , wayBindingMapping = mapRef
                    , wayBindingOutputs = outputs
                    , wayBindingSeats = seats
                    , wayLogFunction = logF
                    , wayExtensibleState = extensible
                    , wayConfig = config
                    , wayFloating = floats
                    , wayEventHook = wayUserConfEventHook conf
                    , wayUserWorkspaces = wayUserConfWorkspaces conf
                    , wayInjectChan = inject
                    , wayCompositor = unsafePerformIO (readIORef compRef)
                    , wayKeybinds = makeBindingMap $ wayUserConfKeybinds conf
                    , wayManagehook = wayUserConfManagehook conf
                    , wayCoreHooks = wayUserConfCoreHooks conf
                    , wayCoreShells = shells
                    }


            let loggers = WayLoggers
                    { loggerOutput = Logger Warn "Output"
                    , loggerWS = Logger Warn "Workspaces"
                    , loggerFocus = Logger Warn "Focus"
                    , loggerXdg = Logger Warn "Xdg_Shell"
                    , loggerX11 = Logger Warn "XWayland"
                    , loggerKeybinds = Logger Warn "Keybindings"
                    , loggerSpawner = Logger Warn "Spawner"
                    , loggerLayout = Logger Warn "Layout"
                    , loggerRender = Logger Warn "Frame"
                    }

            runWay Nothing state (fromMaybe loggers $ configLoggers config) (wayUserRealMain conf compRef)

-- This part is the intended user config (Haskell side)

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
    :: ({-Layouted vs a, ListLike vs a,-} FocusCore vs ws, IsString ws, WSTag ws)
    => [(([WlrModifier], Keysym), KeyBinding (QuadrantSet vs ws) ws)]
bindings =
--    [ (([modi], keysym_k), modifyFocusedWS $ flip _moveFocusLeft)
--    , (([modi], keysym_j), modifyFocusedWS $ flip _moveFocusRight)
--    , (([modi, Shift], keysym_k), modifyFocusedWS $ flip _moveFocusedLeft )
--    , (([modi, Shift], keysym_j), modifyFocusedWS $ flip _moveFocusedRight)
--    , (([modi], keysym_f), sendMessage ToggleFullM)
--    , (([modi], keysym_m), sendMessage ToggleMirror)
--    , (([modi], keysym_Right), sendMessage NextLayout)
    [ (([modi], keysym_k), modifyFocusedWS $ sendToQ TL )
    , (([modi], keysym_j), modifyFocusedWS $ sendToQ TR)
    , (([modi, Shift], keysym_k), modifyFocusedWS $ sendToQ BL)
    , (([modi, Shift], keysym_j), modifyFocusedWS $ sendToQ BR)
    , (([modi], keysym_Return), spawn "alacritty")
    , (([modi], keysym_d), spawn "dmenu_run")
    , (([modi], keysym_w), spawn "vwatch")
    , (([modi], keysym_t), spawn "mpc toggle")
    , (([modi], keysym_n), focusNextOut)
    , (([modi], keysym_q), killCurrent)
    , (([modi], keysym_o), centerFloat)
    , (([modi], keysym_c), doJust getCurrentView makeProxy)
    , (([modi], keysym_a), doJust getCurrentView Multi.copyView)
    , (([modi, Shift], keysym_e), closeCompositor)
    ] ++ concatMap (\(sym, ws) -> [(([modi], sym), greedyView ws), (([modi, Shift], sym), sendTo ws)]) (zip wsSyms workspaces)
    where modi = Alt

myEventHook :: (FocusCore vs a, WSTag a) => SomeEvent -> Way vs a ()
myEventHook =
       H.outputAddHook
    <> idleLog

myConf :: WayUserConf (QuadrantSet (ViewSet Text) Text) Text
myConf = WayUserConf
    { wayUserConfWorkspaces  = workspaces
    , wayUserConfLayouts     = setupQuadrant (sameLayout .  mkMirror $ mkTFull (Tall ||| Spiral))
    , wayUserConfManagehook  = XWay.overrideXRedirect <> manageSpawnOn
    , wayUserConfEventHook   = myEventHook
    , wayUserConfKeybinds    = bindings

    , wayUserConfDisplayHook = [getFuseBracket, getGammaBracket, getFilterBracket filterUser]
    , wayUserConfBackendHook = [getIdleBracket 3e5]
    , wayUserConfPostHook    = [getScreenshooterBracket, envBracket [("PULSE_SERVER", "zelda.ongy")]]
    , wayUserConfCoreHooks   = WayHooks
        { wayHooksVWSChange     = wsScaleHook <> (liftIO . hPrint stderr)
        , wayHooksOutputMapping = enterLeaveHook <> SM.mappingChangeEvt <> (liftIO . hPrint stderr)
          , wayHooksSeatWSChange  = SM.wsChangeLogHook <> handleKeyboardSwitch <> (liftIO . hPrint stderr)
        , wayHooksSeatOutput = SM.outputChangeEvt <> (liftIO . hPrint stderr)
        }
    , wayUserConfShells = [Xdg.makeShell, XWay.makeShell]
    }

main :: IO ()
main = wayUserMain myConf
