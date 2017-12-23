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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main
where

--import Fuse.Main

import qualified Hooks.OutputAdd as H
import WayUtil.View
import WayUtil.Timing
import Hooks.SeatMapping
import Hooks.KeyboardFocus
import Log

import Config

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
import Graphics.Wayland.WlRoots.Screenshooter (screenshooterCreate)
--import Graphics.Wayland.WlRoots.Shell
--    ( WlrShell
--    , --shellCreate
--    )

-- import Compositor
import Input (inputCreate)
import Managehook (Managehook, runQuery, enactInsert, InsertAction (InsertFocused))
import Layout (reLayout)
--import Layout.Full (Full (..))
import Layout.Mirror (Mirror (..), MMessage (..))
import Layout.Tall (Tall (..))
import Layout.ToggleFull (ToggleFull (..), TMessage (..))
import Output (handleOutputAdd, handleOutputRemove)
import Shared (CompHooks (..), ignoreHooks, launchCompositor)
import Utility.Spawn (spawn, spawnManaged, manageNamed, manageSpawnOn, namedSpawner, onSpawner)
import View (View)
import ViewSet
    ( Workspace(..)
    , Layout (..)
    , WSTag
    , contains
    , rmView
    , moveRight
    , moveLeft
    , moveViewLeft
    , moveViewRight
    )
import Waymonad
    ( Way
    , WayStateRef
    , runWay
    , BindingMap
    , KeyBinding
    , WayBindingState (..)
    , makeCallback

    , WayLoggers (..)
    , Logger (..)
    , getViewSet
    )
import Waymonad.Types (Compositor (..))
import WayUtil
    ( sendMessage
    , focusNextOut
    , sendTo
    , killCurrent
    , seatOutputEventHandler
    )
import WayUtil.ViewSet (modifyViewSet, forceFocused, modifyCurrentWS)
import WayUtil.Floating (centerFloat, modifyFloating)
import XWayland (xwayShellCreate, overrideXRedirect)
import XdgShell (xdgShellCreate)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

insertView
    :: WSTag a
    => Managehook a
    -> View
    -> Way a ()
insertView hook v = do
    runQuery v $ enactInsert . flip mappend InsertFocused =<< hook

removeView
    :: (WSTag a)
    => View
    -> Way a ()
removeView v = do
    wsL <- filter (fromMaybe False . fmap (contains v) . wsViews . snd) . M.toList <$> getViewSet

    case wsL of
        [(ws, _)] -> do
            modifyViewSet $ M.adjust (rmView v) ws
            reLayout ws

            forceFocused
        [] -> pure ()
        xs -> liftIO $ do
            hPutStrLn stderr "Found a view in a number of workspaces that's not <2!"
            hPutStrLn stderr $ show $ map fst xs
    modifyFloating (S.delete v)

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

bindings :: DisplayServer -> [(([WlrModifier], Keysym), KeyBinding Text)]
bindings dsp =
    [ (([modi], keysym_k), modifyCurrentWS moveLeft)
    , (([modi], keysym_j), modifyCurrentWS moveRight)
    , (([modi, Shift], keysym_k), modifyCurrentWS moveViewLeft)
    , (([modi, Shift], keysym_j), modifyCurrentWS moveViewRight)
    , (([modi], keysym_Return), spawn "weston-terminal")
    , (([modi, Shift], keysym_Return), spawnManaged dsp [onSpawner "2", namedSpawner "terminal"] "weston-terminal" [])
    , (([modi], keysym_d), spawn "dmenu_run")
    , (([modi], keysym_f), sendMessage TMessage)
    , (([modi], keysym_m), sendMessage MMessage)
    , (([modi], keysym_n), focusNextOut)
    , (([modi], keysym_q), killCurrent)
    , (([modi], keysym_o), centerFloat)
    ] ++ concatMap (\(sym, ws) -> [(([modi], sym), greedyView ws), (([modi, Shift], sym), sendTo ws)]) (zip wsSyms workspaces)
    where modi = Alt

makeBindingMap :: WSTag a => [(([WlrModifier], Keysym), KeyBinding a)] -> BindingMap a
makeBindingMap = M.fromList .
    map (\((mods, Keysym sym), fun) -> ((modifiersToField mods, sym), fun))

makeCompositor
    :: WSTag a
    => IORef DisplayServer
    -> Ptr Backend
    -> (DisplayServer -> [(([WlrModifier], Keysym), KeyBinding a)])
    -> Way a Compositor
makeCompositor dspRef backend keyBinds = do
    display <- liftIO $ readIORef dspRef
    renderer <- liftIO $ rendererCreate backend
    void $ liftIO $ displayInitShm display
    comp <- liftIO $ compositorCreate display renderer
    devManager <- liftIO $ managerCreate display
    layout <- liftIO $ createOutputLayout
    shooter <- liftIO $ screenshooterCreate display renderer

    input <- inputCreate display layout backend (makeBindingMap $ keyBinds display)

    let addFun = insertView (overrideXRedirect <> manageSpawnOn <> manageNamed)
    xdgShell <- xdgShellCreate display addFun removeView
    xway <- xwayShellCreate display comp addFun removeView

--    shell <- pure undefined
    pure $ Compositor
        { compDisplay = display
        , compRenderer = renderer
        , compCompositor = comp
        --, compShell = shell
        , compXdg = xdgShell
        , compManager = devManager
        , compXWayland = xway
        , compBackend = backend
        , compLayout = layout
        , compInput = input
        , compScreenshooter = shooter
        }


defaultMap :: WSTag a => [a] -> IO (WayStateRef a)
defaultMap xs = newIORef $ M.fromList $
    map (, Workspace (Layout (Mirror False (ToggleFull False Tall))) Nothing) xs

realMain :: IORef Compositor -> Way Text ()
realMain compRef = do
    setBaseTime
    dspRef <- liftIO $ newIORef undefined
    compFun <- makeCallback $ \backend -> liftIO . writeIORef compRef =<<  makeCompositor dspRef backend bindings
    outputAdd <- makeCallback $ handleOutputAdd compRef workspaces
    outputRm <- makeCallback $ handleOutputRemove
    --runFuse
    liftIO $ launchCompositor ignoreHooks
        { displayHook = writeIORef dspRef
        , backendPreHook = compFun
        , outputAddHook = outputAdd
        , outputRemoveHook = outputRm
        }

main :: IO ()
main =  do
    config <- loadConfig
    case config of
        Left str -> do
            -- TODO: This should probably be visual later on when possible
            hPutStrLn stderr "Error while loading config:"
            hPutStrLn stderr str
        Right conf -> do
            stateRef  <- defaultMap workspaces
            layoutRef <- newIORef mempty
            mapRef <- newIORef []
            currentRef <- newIORef []
            outputs <- newIORef []
            seats <- newIORef []
            extensible <- newIORef mempty
            floats <- newIORef mempty
            compRef <- newIORef undefined
            logF <- logFun

            let state = WayBindingState
                    { wayBindingCache = layoutRef
                    , wayBindingState = stateRef
                    , wayBindingCurrent = currentRef
                    , wayBindingMapping = mapRef
                    , wayBindingOutputs = outputs
                    , wayBindingSeats = seats
                    , wayLogFunction = logF
                    , wayExtensibleState = extensible
                    , wayConfig = conf
                    , wayFloating = floats
                    , wayEventHook = seatOutputEventHandler
                            <> wsChangeEvtHook
                            <> wsChangeLogHook
                            <> handleKeyboardSwitch
                            <> H.outputAddHook
                    , wayUserWorkspaces = workspaces
                    , wayCompositor = unsafePerformIO (readIORef compRef)
                    }

            let loggers = WayLoggers
                    { loggerOutput = Logger True "Output"
                    , loggerWS = Logger True "Workspaces"
                    , loggerFocus = Logger True "Focus"
                    , loggerXdg = Logger True "Xdg_Shell"
                    , loggerKeybinds = Logger True "Keybindings"
                    , loggerSpawner = Logger True "Spawner"
                    , loggerLayout = Logger True "Layout"
                    }

            runWay Nothing state loggers (realMain compRef)
