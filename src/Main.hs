{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, IORef, writeIORef, readIORef)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text)
import Data.Tuple (swap)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.Server (DisplayServer, displayInitShm)
import System.IO

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

import Compositor
import Input (inputCreate)
import Layout (reLayout)
--import Layout.Full (Full (..))
import Layout.Tall (Tall (..))
import Layout.ToggleFull (ToggleFull (..), TMessage (..))
import Output (handleOutputAdd, handleOutputRemove)
import Shared (CompHooks (..), ignoreHooks, launchCompositor)
import Utility (whenJust)
import View (View)
import ViewSet
    ( Workspace(..)
    , Layout (..)
    , WSTag
    , contains
    , addView
    , rmView
    , moveRight
    , moveLeft
    )
import Waymonad
    ( Way
    , WayStateRef
    , runWay
    , BindingMap
    , KeyBinding
    , getState
    , getSeat
    , WayBindingState (..)
    , makeCallback
    )
import WayUtil
    ( modifyCurrentWS
    , setWorkspace
    , spawn
    , setFoci
    , sendMessage
    , modifyViewSet
    , getViewSet
    , logPutStr
    , focusNextOut
    )
import XWayland (xwayShellCreate)
import XdgShell (xdgShellCreate)

import qualified Data.Map.Strict as M

insertView
    :: WSTag a
    => View
    -> Way a ()
insertView view = do
    logPutStr "Adding new view"
    seat <- getSeat
    state <- getState
    currents <- liftIO . readIORef $ wayBindingCurrent state
    mapping <-  liftIO . readIORef $ wayBindingMapping state
    outputs <-  liftIO . readIORef $ wayBindingOutputs state
    let current = case seat of
            Nothing -> head outputs
            Just s -> fromJust $ M.lookup s $ M.fromList currents
    case M.lookup current . M.fromList $ map swap mapping of
        Nothing -> liftIO $ hPutStrLn stderr "Couldn't lookup workspace for current output"
        Just ws -> do
            modifyViewSet $ M.adjust (addView seat view) ws
            reLayout ws

            vs <- getViewSet
            whenJust (M.lookup ws vs) setFoci

removeView
    :: (WSTag a)
    => View
    -> Way a ()
removeView view = do
    logPutStr "Removing view"
    wsL <- filter (fromMaybe False . fmap (contains view) . wsViews . snd) . M.toList <$> getViewSet

    case wsL of
        [(ws, _)] -> do
            modifyViewSet $ M.adjust (rmView view) ws
            reLayout ws

            state <- getViewSet
            whenJust (M.lookup ws state) setFoci
        xs -> liftIO $ do
            hPutStrLn stderr "Found a view in a number of workspaces that's not 1!"
            hPutStrLn stderr $ show $ map fst xs

bindings :: [(([WlrModifier], Keysym), KeyBinding Text)]
bindings =
    [ (([modi], keysym_j), modifyCurrentWS moveRight)
    , (([modi], keysym_k), modifyCurrentWS moveLeft)
    , (([modi], keysym_1), setWorkspace "1")
    , (([modi], keysym_2), setWorkspace "2")
    , (([modi], keysym_3), setWorkspace "3")
    , (([modi], keysym_4), setWorkspace "4")
    , (([modi], keysym_5), setWorkspace "5")
    , (([modi], keysym_6), setWorkspace "6")
    , (([modi], keysym_7), setWorkspace "7")
    , (([modi], keysym_8), setWorkspace "8")
    , (([modi], keysym_9), setWorkspace "9")
    , (([modi], keysym_0), setWorkspace "0")
    , (([modi], keysym_Return), spawn "weston-terminal")
    , (([modi], keysym_d), spawn "dmenu_run")
    , (([modi], keysym_f), sendMessage TMessage)
    , (([modi], keysym_n), focusNextOut)
    ]
    where modi = Alt

makeBindingMap :: WSTag a => [(([WlrModifier], Keysym), KeyBinding a)] -> BindingMap a
makeBindingMap = M.fromList .
    map (\((mods, Keysym sym), fun) -> ((modifiersToField mods, sym), fun))

makeCompositor
    :: WSTag a
    => IORef DisplayServer
    -> Ptr Backend
    -> [(([WlrModifier], Keysym), KeyBinding a)]
    -> Way a Compositor
makeCompositor dspRef backend keyBinds = do
    display <- liftIO $ readIORef dspRef
    renderer <- liftIO $ rendererCreate backend
    void $ liftIO $ displayInitShm display
    comp <- liftIO $ compositorCreate display renderer
    devManager <- liftIO $ managerCreate display
    layout <- liftIO $ createOutputLayout
    shooter <- liftIO $ screenshooterCreate display renderer

    input <- inputCreate display layout backend (makeBindingMap keyBinds)

    xdgShell <- xdgShellCreate display insertView removeView
    xway <- xwayShellCreate display comp insertView removeView

    shell <- pure undefined
    pure $ Compositor
        { compDisplay = display
        , compRenderer = renderer
        , compCompositor = comp
        , compShell = shell
        , compXdg = xdgShell
        , compManager = devManager
        , compXWayland = xway
        , compBackend = backend
        , compLayout = layout
        , compInput = input
        , compScreenshooter = shooter
        }

workspaces :: [Text]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

defaultMap :: WSTag a => [a] -> IO (WayStateRef a)
defaultMap xs = newIORef $ M.fromList $
    map (, Workspace (Layout (ToggleFull False Tall)) Nothing) xs

realMain :: Way Text ()
realMain = do
    dspRef <- liftIO $ newIORef undefined
    compRef <- liftIO $ newIORef undefined
    compFun <- makeCallback $ \backend -> liftIO . writeIORef compRef =<<  makeCompositor dspRef backend bindings
    outputAdd <- makeCallback $ handleOutputAdd compRef workspaces
    outputRm <- makeCallback $ handleOutputRemove
    liftIO $ launchCompositor ignoreHooks
        { displayHook = writeIORef dspRef
        , backendPreHook = compFun
        , outputAddHook = outputAdd
        , outputRemoveHook = outputRm
        }

main :: IO ()
main =  do
    stateRef  <- defaultMap workspaces
    layoutRef <- newIORef mempty
    mapRef <- newIORef []
    currentRef <- newIORef []
    outputs <- newIORef []
    seats <- newIORef []
    extensible <- newIORef mempty

    let state = WayBindingState
            { wayBindingCache = layoutRef
            , wayBindingState = stateRef
            , wayBindingCurrent = currentRef
            , wayBindingMapping = mapRef
            , wayBindingOutputs = outputs
            , wayBindingSeats = seats
            , wayLogFunction = pure ()
            , wayExtensibleState = extensible
            }

    runWay Nothing state realMain
