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
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Maybe (fromMaybe)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Wayland.Server (DisplayServer, displayInitShm)
import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Compositor (compositorCreate)
import Graphics.Wayland.WlRoots.DeviceManager (managerCreate)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..), modifiersToField)
import Graphics.Wayland.WlRoots.OutputLayout (createOutputLayout)
import Graphics.Wayland.WlRoots.Render.Gles2 (rendererCreate)
import Text.XkbCommon.InternalTypes (Keysym(..))

import Input (inputCreate)
import Output (handleOutputAdd, handleOutputRemove)
import Shared
import ViewSet
import Waymonad (makeCallback)
import Waymonad.Types

-- TODO: Remove
import Config

import qualified Data.Map as M

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
    , wayUserConfLog         :: Way vs ws ()
    }

wayUserRealMain :: (FocusCore vs a, WSTag a) => WayUserConf vs a -> IORef Compositor -> Way vs a ()
wayUserRealMain conf compRef = do
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
            shells <- sequence $ wayUserConfShells conf

            let state = WayBindingState
                    { wayBindingCache = layoutRef
                    , wayBindingState = stateRef
                    , wayBindingCurrent = currentRef
                    , wayBindingMapping = mapRef
                    , wayBindingOutputs = outputs
                    , wayBindingSeats = seats
                    , wayLogFunction = wayUserConfLog conf
                    , wayExtensibleState = extensible
                    , wayConfig = config
                    , wayFloating = floats
                    , wayEventHook = wayUserConfEventHook conf
                    , wayUserWorkspaces = wayUserConfWorkspaces conf
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

