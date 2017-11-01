{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, IORef, writeIORef, readIORef)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Foreign.Ptr (Ptr, ptrToIntPtr, intPtrToPtr)
import Graphics.Wayland.Server (DisplayServer, displayInitShm)
import System.IO

import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Compositor (compositorCreate)
import Graphics.Wayland.WlRoots.DeviceManager (managerCreate)
import Graphics.Wayland.WlRoots.OutputLayout (createOutputLayout)
import Graphics.Wayland.WlRoots.Render.Gles2 (rendererCreate)
--import Graphics.Wayland.WlRoots.Shell
--    ( WlrShell
--    , --shellCreate
--    )

import Compositor
import Input (inputCreate)
import Layout (reLayout)
import Output (handleOutputAdd)
import Shared (CompHooks (..), ignoreHooks, launchCompositor)
import View (View)
import ViewSet (Workspace(..), contains, addView, rmView, Layout (..), Full (..))
import Waymonad (WayState, WayStateRef, LayoutCacheRef, get, modify, runLayoutCache, runWayState)
import XWayland (xwayShellCreate)
import XdgShell (xdgShellCreate)

import qualified Data.Map.Strict as M

intToPtr :: Integral a => a -> Ptr b
intToPtr = intPtrToPtr . fromIntegral

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

insertView
    :: Ord a
    => LayoutCacheRef
    -> IORef a
    -> IORef [(a, Int)]
    -> View
    -> WayState a ()
insertView cacheRef currentWS wsMapping view = do
    mapping <- liftIO $ readIORef wsMapping
    ws <- liftIO $ readIORef currentWS

    modify $ M.adjust (addView view) ws
    reLayout cacheRef ws mapping

removeView
    :: (Ord a, Show a)
    => LayoutCacheRef
    -> IORef [(a, Int)]
    -> View
    -> WayState a ()
removeView cacheRef wsMapping view = do
    mapping <- liftIO $ readIORef wsMapping
    wsL <- filter (fromMaybe False . fmap (contains view) . wsViews . snd) . M.toList <$> get

    case wsL of
        [(ws, _)] -> do
            modify $ M.adjust (rmView view) ws
            reLayout cacheRef ws mapping
        xs -> liftIO $ do
            hPutStrLn stderr "Found a view in a number of workspaces that's not 1!"
            hPutStrLn stderr $ show $ map fst xs


makeCompositor
    :: (Ord a, Show a)
    => DisplayServer
    -> Ptr Backend
    -> LayoutCacheRef
    -> IORef [(a, Int)]
    -> IORef a
    -> WayState a Compositor
makeCompositor display backend ref mappings currentWS = do
    let addFun = insertView ref currentWS mappings
    let delFun = removeView ref mappings
    renderer <- liftIO $ rendererCreate backend
    void $ liftIO $ displayInitShm display
    comp <- liftIO $ compositorCreate display renderer
    devManager <- liftIO $ managerCreate display
--    shell <- liftIO $ shellCreate display
    xdgShell <- xdgShellCreate display   addFun delFun
    xway <- xwayShellCreate display comp addFun delFun
    layout <- liftIO $ createOutputLayout
    input <- runLayoutCache (inputCreate display layout backend) ref
    pure $ Compositor
        { compDisplay = display
        , compRenderer = renderer
        , compCompositor = comp
        , compShell = undefined -- shell
        , compXdg = xdgShell
        , compManager = devManager
        , compXWayland = xway
        , compBackend = backend
        , compLayout = layout
        , compInput = input
        }


defaultMap :: Map Text Workspace
defaultMap = M.fromList [("1", Workspace (Layout Full) Nothing)]

realMain :: IO ()
realMain = do
    stateRef :: WayStateRef Text  <- newIORef defaultMap
    layoutRef <- newIORef mempty
    dpRef <- newIORef undefined
    compRef <- newIORef undefined
    mapRef <- newIORef []
    currentRef <- newIORef "1"
    launchCompositor ignoreHooks
        { displayHook = writeIORef dpRef
        , backendPreHook = \backend -> do
            dsp <- readIORef dpRef
            writeIORef compRef =<< runWayState (makeCompositor dsp backend layoutRef mapRef currentRef) stateRef
          , outputAddHook = handleOutputAdd compRef layoutRef mapRef
        }
    pure ()

main :: IO ()
main = realMain
