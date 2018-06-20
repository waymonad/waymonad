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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Main
where

import System.IO

import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.), shiftL)
import Data.Functor.Identity (Identity)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

import Text.XkbCommon.Keymap (RMLVO)
import Graphics.Wayland.Server (DisplayServer)
import Graphics.Wayland.WlRoots.Backend (Backend, backendGetRenderer)
import Graphics.Wayland.WlRoots.Compositor (compositorCreate)
import Graphics.Wayland.WlRoots.DeviceManager (managerCreate)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..), modifiersToField)
import Graphics.Wayland.WlRoots.OutputLayout (createOutputLayout)
import Graphics.Wayland.WlRoots.Render.Color (Color)
import Text.XkbCommon.InternalTypes (Keysym(..))

import Waymonad.Input (inputCreate)
import Waymonad.Input.Cursor.Type (CursorMapping)
import Waymonad.Output (Output, handleOutputAdd, handleOutputAdd')
import Waymonad.Start
import Waymonad.ViewSet
import Waymonad (makeCallback)
import Waymonad.Types

import qualified Data.IntMap.Strict as IM

makeBindingMap :: [(([WlrModifier], Keysym), KeyBinding vs a)] -> BindingMap vs a
makeBindingMap = IM.fromList .
    map (\((mods, Keysym sym), fun) -> 
        let modPart :: Int = (modifiersToField mods) `shiftL` 32
         in (modPart .|. sym , fun)
        )

makeCompositor
    :: (FocusCore vs ws, WSTag ws)
    => (Ptr InputDevice -> Way vs ws ())
    -> DisplayServer
    -> Ptr Backend
    -> Way vs ws Compositor
makeCompositor inputAdd display backend = do
    liftIO $ hPutStrLn stderr "Creating compositor"
    renderer <- liftIO $ backendGetRenderer backend
    comp <- liftIO $ compositorCreate display renderer
    devManager <- liftIO $ managerCreate display
    layout <- liftIO createOutputLayout

    input <- inputCreate backend inputAdd

    pure Compositor
        { compDisplay = display
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
    { wayUserConfWorkspaces    :: [ws] -- ^List of workspaces. Used mainly for IPC
    , wayUserConfLayouts       :: vs -- ^The initial ViewSet
    , wayUserConfManagehook    :: Managehook vs ws -- ^The Managehook. This is called when a new window is created and has to be managed. Can be used to override the destination.
    , wayUserConfEventHook     :: SomeEvent -> Way vs ws () -- ^Dynamic events emitted by a non-core component.
    , wayUserConfKeybinds      :: [(([WlrModifier], Keysym), KeyBinding vs ws)] -- ^Keybinds attached to every keyboard
    , wayUserConfPointerbinds  :: Way vs ws (CursorMapping Identity) -- ^Pointer attached to every cursor

    , wayUserConfOutputAdd     :: Output -> Way vs ws () -- ^Called everytime an output is added. This should set mode and add to layout if that's wanted.
    , wayUserConfInputAdd      :: Ptr InputDevice -> Way vs ws () -- ^Called verytime an input device is added. This should do any configuration required and attach it to a seat.
    , wayUserConfDisplayHook   :: [Bracketed vs DisplayServer ws] -- ^Early hooks that require the wl_display/DisplayServer to function. Some things in the Way monad may still be undefined
    , wayUserConfBackendHook   :: [Bracketed vs (DisplayServer, Ptr Backend) ws] -- ^Hooks that require the backend for setup. Will have full Way state available.
    , wayUserConfPostHook      :: [Bracketed vs () ws] -- ^Hooks that run after the backend is started. Full Way monad available, and outputs/inputdevices should exist already.
    , wayUserConfCoreHooks     :: WayHooks vs ws -- ^The core events that will be emitted during the runtime of the compositor.
    , wayUserConfShells        :: [IO (WayShell vs ws)] -- ^The shells that should be available. Will be registered and enabled on startup.
    , wayUserConfLog           :: Way vs ws () -- ^The log-function. This can be used to feed a statusbar or similar applications
    , wayUserconfLoggers       :: Maybe WayLoggers
    , wayUserconfColor         :: Color
    , wayUserconfColors        :: Map Text Color
    , wayUserconfFramerHandler :: Maybe (Double -> Output -> Way vs ws ())
    , wayUserconfXKBMap        :: Text -> RMLVO
    }

wayUserRealMain :: (FocusCore vs a, WSTag a) => WayUserConf vs a -> IORef Compositor -> Way vs a ()
wayUserRealMain conf compRef = do
    let outHook = maybe handleOutputAdd handleOutputAdd' $ wayUserconfFramerHandler conf
    outputAdd <- makeCallback $ outHook $ wayUserConfOutputAdd conf

    compFun <- pure $ \(display, backend) -> liftIO . writeIORef compRef =<<  makeCompositor (wayUserConfInputAdd conf) display backend

    launchCompositor ignoreHooks
        { displayHook =  wayUserConfDisplayHook conf
        , backendPreHook = Bracketed compFun (const $ pure ()): wayUserConfBackendHook conf
        , backendPostHook  = wayUserConfPostHook conf

        , outputAddHook    = outputAdd
        }

-- |The intended entry point to the compositor. This will create the Way monad
-- IORefs and static information from the 'WayUserConf' and start things up.
wayUserMain :: (FocusCore vs a, WSTag a) => WayUserConf vs a -> IO ()
wayUserMain conf = do
    stateRef  <- newIORef $ wayUserConfLayouts conf
    mapRef <- newIORef mempty
    currentRef <- newIORef mempty
    outputs <- newIORef mempty
    seats <- newIORef mempty
    lock <- newIORef Nothing
    cancels <- newIORef mempty
    extensible <- newIORef mempty
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
            { wayBindingState = stateRef
            , wayBindingCancels = cancels
            , wayBindingCurrent = currentRef
            , wayBindingMapping = mapRef
            , wayBindingOutputs = outputs
            , wayBindingSeats = seats
            , wayLogFunction = wayUserConfLog conf
            , wayExtensibleState = extensible
            , wayBindingLock = lock
            , wayCurrentSeat = Nothing
            , wayEventHook = wayUserConfEventHook conf
            , wayUserWorkspaces = wayUserConfWorkspaces conf
            , wayCompositor = unsafePerformIO (readIORef compRef)
            , wayKeybinds = makeBindingMap $ wayUserConfKeybinds conf
            , wayPointerbinds = wayUserConfPointerbinds conf
            , wayManagehook = wayUserConfManagehook conf
            , wayCoreHooks = wayUserConfCoreHooks conf
            , wayCoreShells = shells
            , wayLoggers = (fromMaybe loggers $ wayUserconfLoggers conf)
            , wayDefaultColor = wayUserconfColor conf
            , waySeatColors = wayUserconfColors conf
            , wayXKBMap = wayUserconfXKBMap conf
            }


    runWay state (wayUserRealMain conf compRef)
