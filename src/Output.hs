{-# LANGUAGE OverloadedStrings #-}
module Output
    ( handleOutputAdd
    , handleOutputRemove
    )
where

import Control.Exception (bracket_)
import Data.List ((\\))
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, modifyIORef)
import Foreign.Storable (Storable(peek))
import Foreign.Ptr (Ptr, ptrToIntPtr)
import Graphics.Wayland.Server (callbackDone)
import System.IO (stderr)

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.WlRoots.Box (WlrBox(..))
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, setCursorImage)
import Graphics.Wayland.WlRoots.Output
    ( Output
    , getTransMatrix
    , swapOutputBuffers
    , makeOutputCurrent
    , getOutputName
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
import Input (Input(inputXCursor, inputCursor))
import Input.Cursor (cursorRoots)
import Shared (FrameHandler)
import View (View, getViewSurface, renderViewAdditional, getViewBox)
import ViewSet (WSTag (..))
import Waymonad
    ( Way
    , LayoutCacheRef
    , get
    , runLayoutCache'
    , WayBindingState (..)
    , getState
    )

import qualified Data.IntMap.Strict as IM
import qualified Data.Text.IO as T

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


frameHandler
    :: IORef Compositor
    -> LayoutCacheRef
    -> Double
    -> Ptr Output
    -> IO ()
frameHandler compRef cacheRef secs output = runLayoutCache' cacheRef $ do
    views <- get
    comp <- liftIO $ readIORef compRef
    let viewsM = IM.lookup (ptrToInt output) views
    liftIO $ renderOn output (compRenderer comp) $ case viewsM of
        Nothing -> pure ()
        Just wsViews ->
            mapM_ (outputHandleView comp secs output . fst) wsViews

setXCursorImage :: Ptr WlrCursor -> Ptr WlrXCursor -> IO ()
setXCursorImage cursor xcursor = do
    images <- getImages xcursor
    image <- peek $ head images

    setCursorImage
        cursor
        (xCursorImageBuffer image)
        (xCursorImageWidth image)
        (xCursorImageWidth image)
        (xCursorImageHeight image)
        (xCursorImageHotspotX image)
        (xCursorImageHotspotY image)

handleOutputAdd
    :: WSTag a
    => IORef Compositor
    -> [a]
    -> Ptr Output
    -> Way a FrameHandler
handleOutputAdd ref wss output = do
    name <- liftIO $ getOutputName output
    mapRef <- wayBindingMapping <$> getState
    taken <- map fst <$> liftIO (readIORef mapRef)
    liftIO $ case wss \\ taken of
        (x:_) -> do
            T.hPutStr stderr "Attached new output: "
            T.hPutStr stderr name
            T.hPutStrLn stderr ". Attached workspace: "
            T.hPutStrLn stderr $ getName x

            modifyIORef mapRef $ (:) (x, ptrToInt output)
        [] -> do
            T.hPutStrLn stderr "Couldn't pick a workspace for the output"
            T.hPutStr stderr name
            T.hPutStrLn stderr "."
            pure ()

    liftIO $ do
        comp <- readIORef ref

        addOutputAuto (compLayout comp) output
        setXCursorImage
            (cursorRoots $ inputCursor $ compInput comp)
            (inputXCursor $ compInput comp)

    current <- wayBindingOutputs <$> getState
    liftIO $ modifyIORef current ((ptrToInt output) :)

    cacheRef <- wayBindingCache <$> getState

    pure $ \secs out -> frameHandler ref cacheRef secs out

handleOutputRemove
    :: Ptr Output
    -> Way a ()
handleOutputRemove output = do
    state <- getState
    let val = ptrToInt output
    liftIO $ do
        modifyIORef (wayBindingMapping state) $  filter ((/=) val . snd)
        modifyIORef (wayBindingOutputs state) $  \xs -> xs \\ [val]

        name <- getOutputName output
        T.hPutStr stderr "Detached output: "
        T.hPutStr stderr name
        T.hPutStrLn stderr "."
