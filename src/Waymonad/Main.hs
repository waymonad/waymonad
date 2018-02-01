{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2018  Markus Ongyerth

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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Waymonad.Main
where

import System.IO

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Wayland.Server (DisplayServer, displayInitShm)
import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Compositor (compositorCreate)
import Graphics.Wayland.WlRoots.DeviceManager (managerCreate)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..), modifiersToField)
import Graphics.Wayland.WlRoots.OutputLayout (createOutputLayout)
import Graphics.Wayland.WlRoots.Render.Gles2 (rendererCreate)
import Text.XkbCommon.InternalTypes (Keysym(..))

import Input (inputCreate)
import Output (Output, handleOutputAdd, handleOutputRemove)
import Shared
import ViewSet
import Waymonad (makeCallback)
import Waymonad.Types

import qualified Data.Map as M

makeBindingMap :: [(([WlrModifier], Keysym), KeyBinding vs a)] -> BindingMap vs a
makeBindingMap = M.fromList .
    map (\((mods, Keysym sym), fun) -> ((modifiersToField mods, sym), fun))

makeCompositor
    :: (FocusCore vs ws, WSTag ws)
    => (Ptr InputDevice -> Way vs ws ())
    -> DisplayServer
    -> Ptr Backend
    -> Way vs ws Compositor
makeCompositor inputAdd display backend = do
    liftIO $ hPutStrLn stderr "Creating compositor"
    renderer <- liftIO $ rendererCreate backend
    void $ liftIO $ displayInitShm display
    comp <- liftIO $ compositorCreate display renderer
    devManager <- liftIO $ managerCreate display
    layout <- liftIO createOutputLayout

    input <- inputCreate backend inputAdd

    pure Compositor
        { compDisplay = display
        , compRenderer = renderer
        , compCompositor = comp
        , compManager = devManager
        , compBackend = backend
        , compLayout = layout
        , compInput = input
        }

-- | The Main config entry for the compositor.
-- This takes all hooks and common config options as arguments to allow easy
-- access to the compositor functionality.
data WayUserConf vs ws = WayUserConf
    { wayUserConfWorkspaces  :: [ws] -- ^List of workspaces. Used mainly for IPC
    , wayUserConfLayouts     :: [ws] -> vs -- ^Create the initial viewset from the list of userworkspaces
    , wayUserConfManagehook  :: Managehook vs ws -- ^The Managehook. This is called when a new window is created and has to be managed. Can be used to override the destination.
    , wayUserConfEventHook   :: SomeEvent -> Way vs ws () -- ^Dynamic events emitted by a non-core component.
    , wayUserConfKeybinds    :: [(([WlrModifier], Keysym), KeyBinding vs ws)] -- ^Keybinds attached to every keyboard

    , wayUserConfOutputAdd   :: Output -> Way vs ws () -- ^Called everytime an output is added. This should set mode and add to layout if that's wanted.
    , wayUserConfInputAdd    :: Ptr InputDevice -> Way vs ws () -- ^Called verytime an input device is added. This should do any configuration required and attach it to a seat.
    , wayUserConfDisplayHook :: [Bracketed vs DisplayServer ws] -- ^Early hooks that require the wl_display/DisplayServer to function. Some things in the Way monad may still be undefined
    , wayUserConfBackendHook :: [Bracketed vs (DisplayServer, Ptr Backend) ws] -- ^Hooks that require the backend for setup. Will have full Way state available.
    , wayUserConfPostHook    :: [Bracketed vs () ws] -- ^Hooks that run after the backend is started. Full Way monad available, and outputs/inputdevices should exist already.
    , wayUserConfCoreHooks   :: WayHooks vs ws -- ^The core events that will be emitted during the runtime of the compositor.
    , wayUserConfShells      :: [IO (WayShell vs ws)] -- ^The shells that should be available. Will be registered and enabled on startup.
    , wayUserConfLog         :: Way vs ws () -- ^The log-function. This can be used to feed a statusbar or similar applications
    , wayUserconfLoggers     :: Maybe WayLoggers
    }

wayUserRealMain :: (FocusCore vs a, WSTag a) => WayUserConf vs a -> IORef Compositor -> Way vs a ()
wayUserRealMain conf compRef = do
    outputAdd <- makeCallback $ handleOutputAdd $ wayUserConfOutputAdd conf
    outputRm  <- makeCallback handleOutputRemove

    compFun <- pure $ \(display, backend) -> liftIO . writeIORef compRef =<<  makeCompositor (wayUserConfInputAdd conf) display backend

    launchCompositor ignoreHooks
        { displayHook =  wayUserConfDisplayHook conf
        , backendPreHook = Bracketed compFun (const $ pure ()): wayUserConfBackendHook conf
        , backendPostHook  = wayUserConfPostHook conf

        , outputAddHook    = outputAdd
        , outputRemoveHook = outputRm
        }

-- |The intended entry point to the compositor. This will create the Way monad
-- IORefs and static information from the 'WayUserConf' and start things up.
wayUserMain :: (FocusCore vs a, WSTag a) => WayUserConf vs a -> IO ()
wayUserMain conf = do
    stateRef  <- newIORef $ wayUserConfLayouts conf $ wayUserConfWorkspaces conf
    layoutRef <- newIORef mempty
    mapRef <- newIORef []
    currentRef <- newIORef []
    outputs <- newIORef []
    seats <- newIORef []
    extensible <- newIORef mempty
    floats <- newIORef mempty
    currentMap <- newIORef mempty
    compRef <- newIORef $ error "Tried to access compositor to early"
    shells <- sequence $ wayUserConfShells conf

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

    let state = WayBindingState
            { wayBindingCache = layoutRef
            , wayBindingState = stateRef
            , wayBindingCurrent = currentRef
            , wayBindingMapping = mapRef
            , wayBindingOutputs = outputs
            , wayBindingSeats = seats
            , wayLogFunction = wayUserConfLog conf
            , wayExtensibleState = extensible
            , wayFloating = floats
            , wayCurrentSeat = Nothing
            , wayCurrentKeybinds = currentMap
            , wayEventHook = wayUserConfEventHook conf
            , wayUserWorkspaces = wayUserConfWorkspaces conf
            , wayCompositor = unsafePerformIO (readIORef compRef)
            , wayKeybinds = makeBindingMap $ wayUserConfKeybinds conf
            , wayManagehook = wayUserConfManagehook conf
            , wayCoreHooks = wayUserConfCoreHooks conf
            , wayCoreShells = shells
            , wayLoggers = (fromMaybe loggers $ wayUserconfLoggers conf)
            }


    runWay state (wayUserRealMain conf compRef)
