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
module Output
    ( handleOutputAdd
    , handleOutputRemove
    , OutputAddEvent (..)
    )
where

import Control.Exception (bracket_)
import Data.List ((\\), sortOn)
import Data.Ratio (Ratio, (%))
import Control.Monad (when, forM_, forM, filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Foreign.Storable (Storable(peek))
import Foreign.Ptr (Ptr, ptrToIntPtr)
import Graphics.Wayland.Server (callbackDone)
import System.IO (stderr)

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..))
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, setCursorImage)
import Graphics.Wayland.WlRoots.Output
    ( Output
    , OutputMode (..)
    , setOutputMode
    , getModes
    , getTransMatrix
    , swapOutputBuffers
    , makeOutputCurrent
    , getOutputName
    )
import Graphics.Wayland.WlRoots.OutputLayout
    ( WlrOutputLayout
    , addOutput
    , addOutputAuto
    , outputIntersects
-- TODO: I think wlroots made this simpler
    , layoutOuputGetPosition
    , layoutGetOutput
    )
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
import Config (configOutputs)
import qualified Config.Box as C (Point (..))
import Config.Output (OutputConfig (..), Mode (..))
import Input (Input(inputXCursor, inputCursor))
import Input.Cursor (cursorRoots)
import Shared (FrameHandler)
import Utility (whenJust)
import View (View, getViewSurface, renderViewAdditional, getViewBox)
import ViewSet (WSTag (..))
import Waymonad
    ( Way
    , LayoutCacheRef
    , get
    , runLayoutCache'
    , WayBindingState (..)
    , getState
    , EventClass
    , sendEvent
    )
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

outputHandleView :: Compositor -> Double -> Ptr Output -> View -> WlrBox -> IO ()
outputHandleView comp secs output view box = do
    surface <- getViewSurface view
    let x = boxX box
    let y = boxY box
    outputHandleSurface comp secs output surface x y
    renderViewAdditional (outputHandleSurface comp secs output) view


frameHandler
    :: IORef Compositor
    -> LayoutCacheRef
    -> IORef (Set View)
    -> Double
    -> Ptr Output
    -> IO ()
frameHandler compRef cacheRef fRef secs output = runLayoutCache' cacheRef $ do
    comp <- liftIO $ readIORef compRef
    (Point ox oy) <- liftIO (layoutOuputGetPosition =<< layoutGetOutput (compLayout comp) output)
    viewsM <- IM.lookup (ptrToInt output) <$> get
    floats <- filterM (intersects $ compLayout comp) . S.toList =<< liftIO (readIORef fRef)
    liftIO $ renderOn output (compRenderer comp) $ do
        case viewsM of
            Nothing -> pure ()
            Just wsViews ->
                mapM_ (uncurry $ outputHandleView comp secs output) wsViews
        forM_ floats $ \view -> do
            (WlrBox x y w h) <- getViewBox view
            let box = WlrBox (x - ox) (y - oy) w h
            outputHandleView comp secs output view box

    where  intersects layout view = liftIO $ do
                (WlrBox x y w h) <- getViewBox view
                outputIntersects layout output x y (x + w) (y + h)

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

pickMode
    :: MonadIO m
    => Ptr Output
    -> Maybe Mode
    -> m (Maybe (Ptr OutputMode))
-- If there's no config, just pick the "last" mode, it's the native resolution
pickMode output Nothing = liftIO $ do
    modes <- getModes output
    pure $ listToMaybe $ reverse modes
pickMode output (Just cfg) = liftIO $ do
    modes <- getModes output
    pared <- forM modes $ \x -> do
        marshalled <- peek x
        pure (marshalled, x)
    -- First try to find modes that match *exactly* on resolution
    let matches = map snd . sortOn (refreshDist . fst) $ filter (sameResolution . fst) pared
    let ratio = map snd . sortOn (\m -> (resDist $ fst m, refreshDist $ fst m)) $ filter (sameAspect . fst) pared

    -- TODO: Sanitize this
    pure . listToMaybe . reverse $ modes ++ ratio ++ matches
    where   sameResolution :: OutputMode -> Bool
            sameResolution mode =
                fromIntegral (modeWidth mode) == modeCWidth cfg
                && fromIntegral (modeHeight mode) == modeCHeight cfg
            refreshDist :: OutputMode -> Int -- Cast to Int, so we don't get wrapping arithmetic, *should* be big enough!
            refreshDist mode = abs $ fromIntegral (modeRefresh mode) - fromIntegral (modeCRefresh cfg)
            confAspect :: Ratio Word
            confAspect = modeCWidth cfg % modeCHeight cfg
            aspect :: OutputMode -> Ratio Word
            aspect mode = fromIntegral (modeWidth mode) % fromIntegral (modeHeight mode)
            sameAspect :: OutputMode -> Bool
            sameAspect = (==) confAspect . aspect
            resDist :: OutputMode -> Int -- We know it's the same ration, so be lazy here
            resDist mode = abs $ fromIntegral (modeWidth mode) - fromIntegral (modeCWidth cfg)


configureOutput
    :: Ptr WlrOutputLayout
    -> Map Text OutputConfig
    -> Text
    -> Ptr Output
    -> Way a ()
configureOutput layout configs name output = do
    let conf = M.lookup name configs
        position = outPosition =<< conf
        confMode = outMode =<< conf
    liftIO $ case position of
        Nothing -> addOutputAuto layout output
        Just (C.Point x y) -> addOutput layout output x y
    mode <- pickMode output confMode

    liftIO $ whenJust mode (flip setOutputMode output)

newtype OutputAddEvent = OutputAdd Int
    deriving (Eq, Show)

instance EventClass OutputAddEvent

handleOutputAdd
    :: WSTag a
    => IORef Compositor
    -> [a]
    -> Ptr Output
    -> Way a FrameHandler
handleOutputAdd ref wss output = do
    comp <- liftIO $ readIORef ref
    state <- getState
    name <- liftIO $ getOutputName output
    mapRef <- wayBindingMapping <$> getState
    taken <- map fst <$> liftIO (readIORef mapRef)

    configureOutput (compLayout comp) (configOutputs $ wayConfig state) name output

    liftIO $ case wss \\ taken of
        (x:_) -> do
            T.hPutStr stderr "Added output: "
            T.hPutStr stderr name
            T.hPutStr stderr ". Attached workspace: "
            T.hPutStrLn stderr $ getName x

            modifyIORef mapRef $ (:) (x, ptrToInt output)
        [] -> do
            T.hPutStrLn stderr "Couldn't pick a workspace for the output"
            T.hPutStr stderr name
            T.hPutStrLn stderr "."
            pure ()

    liftIO $ setXCursorImage
            (cursorRoots $ inputCursor $ compInput comp)
            (inputXCursor $ compInput comp)

    current <- wayBindingOutputs <$> getState
    liftIO $ modifyIORef current ((ptrToInt output) :)

    cacheRef <- wayBindingCache <$> getState
    floats <- wayFloating <$> getState

    sendEvent $ OutputAdd $ ptrToInt output

    pure $ \secs out -> frameHandler ref cacheRef floats secs out

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
