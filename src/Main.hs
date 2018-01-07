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
module Main
where

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
import Layout.Mirror (Mirror (..), MMessage (..))
import Layout.Tall (Tall (..))
import Layout.ToggleFull (ToggleFull (..), TMessage (..))
import Output (handleOutputAdd, handleOutputRemove)
import Shared (CompHooks (..), ignoreHooks, launchCompositor, Bracketed (..))
import Utility (doJust)
import Utility.Spawn (spawn, manageNamed, manageSpawnOn, spawnOn)
import qualified View.Multi as Multi
import View.Proxy (makeProxy)
import ViewSet
    ( Workspace(..)
    , Layout (..)
    , WSTag
    , LayoutClass
    , moveRight
    , moveLeft
    , moveViewLeft
    , moveViewRight
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

workspaces :: [Text]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

bindings :: [(([WlrModifier], Keysym), KeyBinding Text)]
bindings =
    [ (([modi], keysym_k), modifyFocusedWS moveLeft)
    , (([modi], keysym_j), modifyFocusedWS moveRight)
    , (([modi, Shift], keysym_k), modifyFocusedWS moveViewLeft)
    , (([modi, Shift], keysym_j), modifyFocusedWS moveViewRight)
    , (([modi], keysym_Return), spawn "weston-terminal")
    , (([modi, Shift], keysym_Return), spawnOn "2" "weston-terminal" [])
    , (([modi], keysym_d), spawn "dmenu_run")
    , (([modi], keysym_f), sendMessage TMessage)
    , (([modi], keysym_m), sendMessage MMessage)
    , (([modi], keysym_n), focusNextOut)
    , (([modi], keysym_q), killCurrent)
    , (([modi], keysym_o), centerFloat)
    , (([modi], keysym_Right), sendMessage NextLayout)
    , (([modi], keysym_c), doJust getCurrentView makeProxy)
    , (([modi], keysym_a), doJust getCurrentView Multi.copyView)
    , (([modi, Shift], keysym_e), closeCompositor)
    ] ++ concatMap (\(sym, ws) -> [(([modi], sym), greedyView ws), (([modi, Shift], sym), sendTo ws)]) (zip wsSyms workspaces)
    where modi = Alt

makeBindingMap :: WSTag a => [(([WlrModifier], Keysym), KeyBinding a)] -> BindingMap a
makeBindingMap = M.fromList .
    map (\((mods, Keysym sym), fun) -> ((modifiersToField mods, sym), fun))

makeCompositor
    :: WSTag a
    => DisplayServer
    -> Ptr Backend
    -> Way a Compositor
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

sameLayout :: (WSTag a, LayoutClass l) => l -> [a] -> M.Map a Workspace
sameLayout l = M.fromList . map (, Workspace (Layout (l)) Nothing)

data WayUserConf a = WayUserConf
    { wayUserConfWorkspaces  :: [a]
    , wayUserConfLayouts     :: [a] -> M.Map a Workspace
    , wayUserConfManagehook  :: Managehook a
    , wayUserConfEventHook   :: SomeEvent -> Way a ()
    , wayUserConfKeybinds    :: [(([WlrModifier], Keysym), KeyBinding a)]

    , wayUserConfDisplayHook :: [Way a (Bracketed DisplayServer)]
    , wayUserConfBackendHook :: [Way a (Bracketed (DisplayServer, Ptr Backend))]
    , wayUserConfPostHook    :: [Way a (Bracketed ())]
    }

wayUserRealMain :: WSTag a => WayUserConf a -> IORef Compositor -> Way a ()
wayUserRealMain conf compRef = do
    setBaseTime
    displayBrackets <- sequence $ wayUserConfDisplayHook conf
    backendBrackets <- sequence $ wayUserConfBackendHook conf
    postBrackets <- sequence $ wayUserConfPostHook conf

    outputAdd <- makeCallback $ handleOutputAdd compRef $ wayUserConfWorkspaces conf
    outputRm <- makeCallback handleOutputRemove

    compFun <- makeCallback $ \(display, backend) -> liftIO . writeIORef compRef =<<  makeCompositor display backend

    liftIO $ launchCompositor ignoreHooks
        { displayHook = displayBrackets
        , backendPreHook = Bracketed compFun (const $ pure ()): backendBrackets
        , backendPostHook = postBrackets
        , outputAddHook = outputAdd
        , outputRemoveHook = outputRm
        }

wayUserMain :: WSTag a => WayUserConf a -> IO ()
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

myEventHook :: WSTag a => SomeEvent -> Way a ()
myEventHook = seatOutputEventHandler
    <> wsChangeEvtHook
    <> wsChangeLogHook
    <> handleKeyboardSwitch
    <> H.outputAddHook
    <> enterLeaveHook
    <> wsScaleHook
    <> idleLog

myConf :: WayUserConf Text
myConf = WayUserConf
    { wayUserConfWorkspaces  = workspaces
    , wayUserConfLayouts     = sameLayout (Mirror False (ToggleFull False (Tall ||| Spiral)))
    , wayUserConfManagehook  = overrideXRedirect <> manageSpawnOn <> manageNamed
    , wayUserConfEventHook   = myEventHook
    , wayUserConfKeybinds    = bindings

    , wayUserConfDisplayHook = [getFuseBracket, getGammaBracket]
    , wayUserConfBackendHook = [getIdleBracket 3e5]
    , wayUserConfPostHook    = [getScreenshooterBracket]
    }

main :: IO ()
main = wayUserMain myConf
