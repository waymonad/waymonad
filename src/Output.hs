{-# LANGUAGE OverloadedStrings #-}
module Output
    ( handleOutputAdd
    )
where

import Control.Exception (bracket_)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, writeIORef, readIORef)
import Data.Text (Text)
import Foreign.Storable (Storable(peek))
import Foreign.Ptr (Ptr, ptrToIntPtr)
import Graphics.Wayland.Server (callbackDone)
import System.IO (hPutStrLn, stderr)

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.WlRoots.Box (WlrBox(..))
import Graphics.Wayland.WlRoots.Output
    ( Output
    , getTransMatrix
    , swapOutputBuffers
    , makeOutputCurrent
    , setCursor
    )
import Graphics.Wayland.WlRoots.OutputLayout (addOutputAuto)
import Graphics.Wayland.WlRoots.Render
    ( Renderer
    , renderWithMatrix
    , isTextureValid
    , doRender
    )
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixTranslate)
import Graphics.Wayland.WlRoots.Surface
    ( WlrSurface
    , surfaceGetSubs
    , subSurfaceGetBox
    , subSurfaceGetSurface
    , getCurrentState
    , callbackGetCallback
    , callbackGetResource
    , surfaceGetCallbacks
    , withSurfaceMatrix
    , surfaceGetTexture
    )
import Graphics.Wayland.WlRoots.XCursor
    ( WlrXCursor
    , getImages
    , WlrXCursorImage (..)
    )

import Compositor
import Input (Input(inputXCursor))
import Shared (FrameHandler)
import View (View, getViewSurface, renderViewAdditional, getViewBox)
import Waymonad (LayoutCacheRef, LayoutCache, get, runLayoutCache)

import qualified Data.IntMap.Strict as IM

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
        Just wsViews -> liftIO $ do
            comp <- readIORef compRef
            renderOn output (compRenderer comp) $ do
                mapM_ (outputHandleView comp secs output . fst) wsViews

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
