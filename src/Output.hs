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
module Output
    ( handleOutputAdd
    , handleOutputRemove
    , OutputAddEvent (..)
    )
where

import Control.Exception (bracket_)
import Data.List ((\\), sortOn)
import Data.Ratio (Ratio, (%))
import Control.Monad (forM_, forM, filterM, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Foreign.Storable (Storable(peek))
import Foreign.Ptr (Ptr, ptrToIntPtr)
import Graphics.Wayland.Server (callbackDone)
import System.IO (hPutStrLn, stderr)

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..))
import Graphics.Wayland.WlRoots.Output
    ( WlrOutput
    , OutputMode (..)
    , setOutputMode
    , getModes
    , getTransMatrix
    , swapOutputBuffers
    , makeOutputCurrent
    , getOutputName
    , getOutputScale
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
    , getTextureSize
    )
import Graphics.Wayland.WlRoots.Render.Matrix
    ( withMatrix
    , matrixTranslate
    , matrixScale
    , matrixMul
    )
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

import Waymonad.Types (Compositor (..))
import Config (configOutputs)
import qualified Config.Box as C (Point (..))
import Config.Output (OutputConfig (..), Mode (..))
--import Input (Input(inputXCursor, inputCursor))
--import Input.Cursor (cursorRoots)
import Input.Seat (Seat(seatLoadScale))
import Shared (FrameHandler)
import Utility (whenJust)
import View (View, getViewSurface, renderViewAdditional, getViewBox, viewGetScale, viewGetLocal)
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
import WayUtil (getSeats)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.IO as T

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

renderOn :: Ptr WlrOutput -> Ptr Renderer -> IO a -> IO a
renderOn output rend act = bracket_
    (makeOutputCurrent output)
    (swapOutputBuffers output)
    (doRender rend output act)

outputHandleSurface :: Compositor -> Double -> Ptr WlrOutput -> Ptr WlrSurface -> Float -> WlrBox -> IO ()
outputHandleSurface comp secs output surface scaleFactor box = do
    texture <- surfaceGetTexture surface
    isValid <- isTextureValid texture
    when isValid $ withMatrix $ \trans -> withMatrix $ \scale -> withMatrix $ \final -> do
            (twidth, theight) <- getTextureSize texture
            let x = boxX box
                y = boxY box
                bwidth = boxWidth box
                bheight = boxHeight box

            matrixTranslate trans (realToFrac x) (realToFrac y) 0
            matrixScale scale scaleFactor scaleFactor 1
            matrixMul trans scale final
            withSurfaceMatrix surface (getTransMatrix output) final $ \mat -> do
                renderWithMatrix (compRenderer comp) texture mat

            callbacks <- surfaceGetCallbacks =<< getCurrentState surface
            forM_ callbacks $ \callback -> do
                cb <- callbackGetCallback callback
                callbackDone cb (floor $ secs * 1000)
                res <- callbackGetResource callback
                resourceDestroy res

            subs <- surfaceGetSubs surface
            forM_ subs $ \sub -> do
                sbox <- subSurfaceGetBox sub
                subsurf <- subSurfaceGetSurface sub
                outputHandleSurface
                    comp
                    secs
                    output
                    subsurf
                    scaleFactor
                    sbox{ boxX = boxX sbox + boxX box
                        , boxY = boxY sbox + boxY box
                        , boxWidth = floor $ fromIntegral (boxWidth sbox) * scaleFactor
                        , boxHeight = floor $ fromIntegral (boxHeight sbox) * scaleFactor
                        }

outputHandleView :: Compositor -> Double -> Ptr WlrOutput -> View -> WlrBox -> IO (IO ())
outputHandleView comp secs output view box = do
    surface <- getViewSurface view
    scale <- viewGetScale view
    local <- viewGetLocal view
    let lBox = box { boxX = boxX box + boxX local, boxY = boxY box + boxY local}
    outputHandleSurface comp secs output surface scale lBox
    pure $ renderViewAdditional (\v b ->
        void $ outputHandleSurface
            comp
            secs
            output
            v
            scale
            b   { boxX = floor (fromIntegral (boxX b) * scale) + boxX lBox
                , boxY = floor (fromIntegral (boxY b) * scale) + boxY lBox
                , boxWidth = floor $ fromIntegral (boxWidth b) * scale
                , boxHeight = floor $ fromIntegral (boxHeight b) * scale
                }
        )
        view


frameHandler
    :: IORef Compositor
    -> LayoutCacheRef
    -> IORef (Set View)
    -> Double
    -> Ptr WlrOutput
    -> IO ()
frameHandler compRef cacheRef fRef secs output = runLayoutCache' cacheRef $ do
    comp <- liftIO $ readIORef compRef
    (Point ox oy) <- liftIO (layoutOuputGetPosition =<< layoutGetOutput (compLayout comp) output)
    viewsM <- IM.lookup (ptrToInt output) <$> get
    floats <- filterM (intersects $ compLayout comp) . S.toList =<< liftIO (readIORef fRef)
    liftIO $ renderOn output (compRenderer comp) $ do
        case viewsM of
            Nothing -> pure ()
            Just wsViews -> do
                overs <- mapM (uncurry $ outputHandleView comp secs output) wsViews
                sequence_ overs
        forM_ floats $ \view -> do
            (WlrBox x y w h) <- getViewBox view
            let box = WlrBox (x - ox) (y - oy) w h
            outputHandleView comp secs output view box

    where  intersects layout view = liftIO $ do
                (WlrBox x y w h) <- getViewBox view
                outputIntersects layout output x y (x + w) (y + h)

pickMode
    :: MonadIO m
    => Ptr WlrOutput
    -> Maybe Mode
    -> m (Maybe (Ptr OutputMode))
-- If there's no config, just pick the "last" mode, it's the native resolution
pickMode output Nothing = liftIO $ do
    modes <- getModes output
    pure $ listToMaybe $ reverse modes
pickMode output (Just cfg) = liftIO $ do
    modes <- getModes output
    paired <- forM modes $ \x -> do
        marshalled <- peek x
        pure (marshalled, x)
    -- First try to find modes that match *exactly* on resolution
    let matches = map snd . sortOn (refreshDist . fst) $ filter (sameResolution . fst) paired
    let ratio = map snd . sortOn (\m -> (resDist $ fst m, refreshDist $ fst m)) $ filter (sameAspect . fst) paired

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
    -> Ptr WlrOutput
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
    -> Ptr WlrOutput
    -> Way a FrameHandler
handleOutputAdd ref _ output = do
    comp <- liftIO $ readIORef ref
    state <- getState
    name <- liftIO $ getOutputName output

    configureOutput (compLayout comp) (configOutputs $ wayConfig state) name output

    current <- wayBindingOutputs <$> getState
    liftIO $ modifyIORef current ((ptrToInt output) :)

    cacheRef <- wayBindingCache <$> getState
    floats <- wayFloating <$> getState

    scale <- liftIO $ getOutputScale output
    seats <- getSeats
    liftIO $ forM_ seats $ \seat -> seatLoadScale seat scale

    sendEvent $ OutputAdd $ ptrToInt output

    pure $ \secs out -> frameHandler ref cacheRef floats secs out

handleOutputRemove
    :: Ptr WlrOutput
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
