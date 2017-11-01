{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main
where

import Data.Maybe (fromMaybe)
import System.IO
import Data.Text (Text)
import Foreign.Storable (Storable(peek))
import XdgShell
    ( XdgShell
    , xdgShellCreate
    )
import XWayland
    ( XWayShell
    , xwayShellCreate
    )

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Waymonad
import Shared
import View
import ViewSet

import Control.Monad.IO.Class (liftIO)
import Input (Input(..), inputCreate)
import Foreign.Ptr (Ptr, ptrToIntPtr, intPtrToPtr)
import Data.IORef (newIORef, IORef, writeIORef, readIORef, modifyIORef)

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixTranslate)
import Graphics.Wayland.WlRoots.Box (WlrBox (..))
import Graphics.Wayland.WlRoots.Render
    ( Renderer
    , doRender
    , isTextureValid
    , renderWithMatrix
    )
import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.XCursor
    ( WlrXCursor
    , getImages
    , WlrXCursorImage (..)
    )
import Graphics.Wayland.WlRoots.Render.Gles2 (rendererCreate)
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor, compositorCreate)
import Graphics.Wayland.WlRoots.Shell
    ( WlrShell
    , --shellCreate
    )
import Graphics.Wayland.WlRoots.DeviceManager
    ( WlrDeviceManager
    , managerCreate
    )
import Graphics.Wayland.WlRoots.OutputLayout
    ( WlrOutputLayout
    , createOutputLayout
    , addOutputAuto
    )
import Graphics.Wayland.WlRoots.Output
    ( Output
    , makeOutputCurrent
    , swapOutputBuffers
    , getTransMatrix
    , setCursor
    , getOutputBox
    )
import Graphics.Wayland.WlRoots.Surface
    ( surfaceGetTexture
    , withSurfaceMatrix
    , callbackGetResource
    , surfaceGetCallbacks
    , callbackGetCallback
    , getCurrentState
    , WlrSurface

    , subSurfaceGetSurface
    , surfaceGetSubs
    , subSurfaceGetBox
    )
import Graphics.Wayland.Server
    ( displayInitShm
    , DisplayServer
    , callbackDone
    )
import Control.Exception (bracket_)
import Control.Monad (void, when, forM_)

import Data.Map (Map)

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

data Compositor = Compositor
    { compDisplay :: DisplayServer
    , compRenderer :: Ptr Renderer
    , compCompositor :: Ptr WlrCompositor
    , compShell :: Ptr WlrShell
    , compXdg :: XdgShell
    , compManager :: Ptr WlrDeviceManager
    , compXWayland :: XWayShell
    , compBackend :: Ptr Backend
    , compLayout :: Ptr WlrOutputLayout
    , compInput :: Input
    }

intToPtr :: Integral a => a -> Ptr b
intToPtr = intPtrToPtr . fromIntegral

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

renderOn :: Ptr Output -> Ptr Renderer -> IO a -> IO a
renderOn output rend act = bracket_
    (makeOutputCurrent output)
    (swapOutputBuffers output)
    (doRender rend output act)

outputHandleSurface :: Compositor -> Double -> Ptr Output -> Ptr WlrSurface -> Int -> Int -> IO ()
outputHandleSurface comp secs output surface x y = do
    texture <- surfaceGetTexture surface
    isValid <- isTextureValid texture
    when isValid $ withMatrix $ \trans -> do
        matrixTranslate trans (realToFrac x) (realToFrac y) 0
        withSurfaceMatrix surface (getTransMatrix output) trans $ \mat -> do
            renderWithMatrix (compRenderer comp) texture mat

        callbacks <- surfaceGetCallbacks =<< getCurrentState surface
        forM_ callbacks $ \callback -> do
            cb <- callbackGetCallback callback
            callbackDone cb (floor $ secs * 1000)
            res <- callbackGetResource callback
            resourceDestroy res

        subs <- surfaceGetSubs surface
        forM_ subs $ \sub -> do
            box <- subSurfaceGetBox sub
            subsurf <- subSurfaceGetSurface sub
            outputHandleSurface comp secs output subsurf (x + boxX box) (y + boxY box)

outputHandleView :: Compositor -> Double -> Ptr Output -> View -> IO ()
outputHandleView comp secs output view = do
    surface <- getViewSurface view
    box <- getViewBox view
    let x = boxX box
    let y = boxY box
    outputHandleSurface comp secs output surface x y
    renderViewAdditional (outputHandleSurface comp secs output) view


frameHandler :: IORef Compositor -> Double -> Ptr Output -> LayoutCache ()
frameHandler compRef secs output = do
    views <- get
    let viewsM = IM.lookup (ptrToInt output) views
    case viewsM of
        Nothing -> pure ()
        Just views -> liftIO $ do
            comp <- readIORef compRef
            renderOn output (compRenderer comp) $ do
                mapM_ (outputHandleView comp secs output . fst) views

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

reLayout :: Ord a => LayoutCacheRef -> a -> [(a, Int)] -> WayState a ()
reLayout cacheRef ws xs = do
    wstate <- M.lookup ws <$> get

    liftIO $ whenJust (M.lookup ws $ M.fromList xs) $ \out -> whenJust wstate $ \case
        (Workspace _ Nothing) -> modifyIORef cacheRef $ IM.delete out
        (Workspace (Layout l) (Just vs)) -> do
            box <- getOutputBox $ intToPtr out
            let layout = pureLayout l box vs
            modifyIORef cacheRef $ IM.insert out layout

            mapM_ (uncurry setViewBox) layout

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

setCursorImage :: Ptr Output -> Ptr WlrXCursor -> IO ()
setCursorImage output xcursor = do
    images <- getImages xcursor
    image <- peek $ head images

    setCursor
        output
        (xCursorImageBuffer image)
        (xCursorImageWidth image)
        (xCursorImageWidth image)
        (xCursorImageHeight image)
        (xCursorImageHotspotX image)
        (xCursorImageHotspotY image)

handleOutputAdd
    :: IORef Compositor
    -> LayoutCacheRef
    -> IORef [(Text, Int)]
    -> Ptr Output 
    -> IO FrameHandler
handleOutputAdd ref stateRef mapRef output = do
    hPutStrLn stderr "Found output"
    writeIORef mapRef [("1", ptrToInt output)]
    comp <- readIORef ref

    setCursorImage output (inputXCursor $ compInput comp)
    hPutStrLn stderr "Set cursor image"
    addOutputAuto (compLayout comp) output
    hPutStrLn stderr "Added output to layout "

    pure $ \secs out ->
        runLayoutCache (frameHandler ref secs out) stateRef

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
