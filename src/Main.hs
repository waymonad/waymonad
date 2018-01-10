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

import StartupHook
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
import Hooks.SeatMapping
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
import Utility.Spawn (spawn, manageNamed, manageSpawnOn, spawnOn)
import qualified View.Multi as Multi
import View.Proxy (makeProxy)
import ViewSet
    ( Workspace(..)
    , GenericLayout (..)
    , WSTag
    , FocusCore
    , GenericLayoutClass
    , moveRight
    , moveLeft
    , moveViewLeft
    , moveViewRight
    , ViewSet
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
import Waymonad.Types (Compositor (..), LogPriority (..), Managehook, SomeEvent)
import WayUtil
    ( sendMessage
    , focusNextOut
    , sendTo
    , killCurrent
    , seatOutputEventHandler
    , closeCompositor
    )
import WayUtil.Current (getCurrentView)
import WayUtil.ViewSet (modifyFocusedWS)
import WayUtil.Floating (centerFloat)
import XWayland (xwayShellCreate, overrideXRedirect)
import XdgShell (xdgShellCreate)

import qualified Data.Map.Strict as M


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

    xdgShell <- xdgShellCreate display
    xway <- xwayShellCreate display comp
    pure Compositor
        { compDisplay = display
        , compRenderer = renderer
        , compCompositor = comp
        , compXdg = xdgShell
        , compManager = devManager
        , compXWayland = xway
        , compBackend = backend
        , compLayout = layout
        , compInput = input
        }

sameLayout
    :: (WSTag a, GenericLayoutClass l (ViewSet a) a)
    => l -> [a] -> M.Map a (Workspace a)
sameLayout l = M.fromList . map (, Workspace (GenericLayout (l)) Nothing)

data WayUserConf vs a = WayUserConf
    { wayUserConfWorkspaces  :: [a]
    , wayUserConfLayouts     :: [a] -> vs
    , wayUserConfManagehook  :: Managehook vs a
    , wayUserConfEventHook   :: SomeEvent -> Way vs a ()
    , wayUserConfKeybinds    :: [(([WlrModifier], Keysym), KeyBinding vs a)]

    , wayUserConfDisplayHook :: [Bracketed vs DisplayServer a]
    , wayUserConfBackendHook :: [Bracketed vs (DisplayServer, Ptr Backend) a]
    , wayUserConfPostHook    :: [Bracketed vs () a]
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
    ]

workspaces :: IsString a => [a]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

bindings :: (FocusCore vs a, IsString a, WSTag a) => [(([WlrModifier], Keysym), KeyBinding vs a)]
bindings =
--    [ (([modi], keysym_k), modifyFocusedWS moveLeft)
--    , (([modi], keysym_j), modifyFocusedWS moveRight)
--    , (([modi, Shift], keysym_k), modifyFocusedWS moveViewLeft)
--    , (([modi, Shift], keysym_j), modifyFocusedWS moveViewRight)
    [ (([modi], keysym_Return), spawn "weston-terminal")
    , (([modi, Shift], keysym_Return), spawnOn "2" "weston-terminal" [])
    , (([modi], keysym_d), spawn "dmenu_run")
    , (([modi], keysym_f), sendMessage ToggleFullM)
    , (([modi], keysym_m), sendMessage ToggleMirror)
    , (([modi], keysym_n), focusNextOut)
    , (([modi], keysym_q), killCurrent)
    , (([modi], keysym_o), centerFloat)
    , (([modi], keysym_Right), sendMessage NextLayout)
    , (([modi], keysym_c), doJust getCurrentView makeProxy)
    , (([modi], keysym_a), doJust getCurrentView Multi.copyView)
    , (([modi, Shift], keysym_e), closeCompositor)
    ] ++ concatMap (\(sym, ws) -> [(([modi], sym), greedyView ws), (([modi, Shift], sym), sendTo ws)]) (zip wsSyms workspaces)
    where modi = Alt

myEventHook :: (FocusCore vs a, WSTag a) => SomeEvent -> Way vs a ()
myEventHook =
       seatOutputEventHandler
    <> wsChangeEvtHook
    <> wsChangeLogHook
    <> handleKeyboardSwitch
    <> H.outputAddHook
    <> enterLeaveHook
    <> wsScaleHook
    <> idleLog

myConf :: WayUserConf (ViewSet Text) Text
myConf = WayUserConf
    { wayUserConfWorkspaces  = workspaces
    , wayUserConfLayouts     = sameLayout {-mkMirror $ mkTFull (Tall ||| Spiral) -} Tall
    , wayUserConfManagehook  = overrideXRedirect <> manageSpawnOn
    , wayUserConfEventHook   = myEventHook
    , wayUserConfKeybinds    = bindings

    , wayUserConfDisplayHook = [getFuseBracket, getGammaBracket, getFilterBracket filterUser, getStartupBracket (spawn "weston-terminal")]
    , wayUserConfBackendHook = [getIdleBracket 3e5]
    , wayUserConfPostHook    = [getScreenshooterBracket]
    }

main :: IO ()
main = wayUserMain myConf
