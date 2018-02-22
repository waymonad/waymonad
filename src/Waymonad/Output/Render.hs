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
{-# LANGUAGE BangPatterns #-}
module Waymonad.Output.Render
where

import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.Server
    ( outputTransformFlipped_180, callbackDone)

import Graphics.Pixman
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), boxTransform, scaleBox, centerBox)
import Graphics.Wayland.WlRoots.Output
    ( WlrOutput, getOutputDamage , isOutputEnabled, getOutputNeedsSwap
    , invertOutputTransform, getOutputTransform
    , outputTransformedResolution, composeOutputTransform, getTransMatrix
      , swapOutputBuffers, getOutputScale, makeOutputCurrent
    )
import Graphics.Wayland.WlRoots.Render
    ( Renderer, rendererScissor, rendererClear, renderWithMatrix, isTextureValid
    , doRender, getMatrix
    )
import Graphics.Wayland.WlRoots.Render.Color (Color (..))
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixProjectBox)
import Graphics.Wayland.WlRoots.Surface
    ( WlrSurface, subSurfaceGetSurface, surfaceGetSubs, subSurfaceGetBox
    , surfaceGetCallbacks, getCurrentState, callbackGetCallback
    , callbackGetResource, surfaceGetTexture, surfaceGetTransform
    )

import Waymonad (getState, makeCallback , unliftWay)
import Waymonad.Types
    ( Way, Output (..), SSDPrio
    , Compositor (..)
    , WayBindingState (..)
    )
import Waymonad.Types.Core (View)
import Waymonad.View
    ( viewHasCSD, viewGetLocal, getViewSurface, renderViewAdditional
    , viewGetScale
    )
import Waymonad.Utility.Extensible
import Waymonad.Utility.SSD (renderDeco, getDecoBox)
import Waymonad.Utility.Base (doJust)
import Waymonad.ViewSet (WSTag)
import Waymonad.Output.Background

renderOn :: Ptr WlrOutput -> Ptr Renderer -> (Int -> Way vs ws a) -> Way vs ws (Maybe a)
renderOn output rend act = doJust (liftIO $ makeOutputCurrent output) $ \age -> do
    ret <- liftIO . doRender rend output =<< unliftWay (act age)
    void . liftIO $ swapOutputBuffers output Nothing
    pure $ Just ret

renderDamaged :: Ptr Renderer -> Ptr WlrOutput -> PixmanRegion32 -> WlrBox -> IO () -> IO ()
renderDamaged render output damage box act = do
    outputScale <- getOutputScale output
    withRegion $ \region -> do
        resetRegion region $ Just $ scaleBox box outputScale
        pixmanRegionIntersect region damage
        boxes <- pixmanRegionBoxes region
        forM_ boxes $ \pbox -> do
            scissorOutput render output $ boxToWlrBox pbox
            act

outputHandleSurface :: Compositor -> Double -> Ptr WlrOutput -> PixmanRegion32 -> Ptr WlrSurface -> Float -> WlrBox -> IO ()
outputHandleSurface comp secs output damage surface scaleFactor baseBox@(WlrBox !bx !by !bw !bh) = do
    outputScale <- getOutputScale output
    texture <- surfaceGetTexture surface
    isValid <- isTextureValid texture
    when isValid $ do
            let surfBox = WlrBox
                    (floor $ fromIntegral bx * outputScale)
                    (floor $ fromIntegral by * outputScale)
                    (ceiling $ fromIntegral bw * outputScale)
                    (ceiling $ fromIntegral bh * outputScale)

            withMatrix $ \mat -> do
                surfTransform <- invertOutputTransform <$> surfaceGetTransform surface
                matrixProjectBox mat surfBox surfTransform 0 (getTransMatrix output)
                renderDamaged (compRenderer comp) output damage baseBox $ 
                    renderWithMatrix (compRenderer comp) texture mat

            subs <- surfaceGetSubs surface
            forM_ subs $ \sub -> do
                sbox <- subSurfaceGetBox sub
                subsurf <- subSurfaceGetSurface sub
                outputHandleSurface
                    comp
                    secs
                    output
                    damage
                    subsurf
                    scaleFactor
                    sbox{ boxX = floor (fromIntegral (boxX sbox) * scaleFactor) + bx
                        , boxY = floor (fromIntegral (boxY sbox) * scaleFactor) + by
                        , boxWidth = floor $ fromIntegral (boxWidth sbox) * scaleFactor
                        , boxHeight = floor $ fromIntegral (boxHeight sbox) * scaleFactor
                        }

outputHandleView :: Compositor -> Double -> Ptr WlrOutput -> PixmanRegion32 -> (View, SSDPrio, WlrBox) -> Way vs ws (IO ())
outputHandleView comp secs output d (!view, !prio, !obox) = doJust (getViewSurface view) $ \surface -> do
    hasCSD <- viewHasCSD view
    let box = getDecoBox hasCSD prio obox
    scale <- liftIO $ viewGetScale view
    local <- liftIO $ viewGetLocal view
    let lBox = local { boxX = boxX box + boxX local, boxY = boxY box + boxY local}

    decoCB <- unliftWay $ renderDeco hasCSD prio output obox box
    liftIO $ renderDamaged (compRenderer comp) output d obox decoCB

    liftIO $ outputHandleSurface comp secs output d surface scale lBox
    pure $ renderViewAdditional (\s b ->
        void $ outputHandleSurface
            comp
            secs
            output
            d
            s
            scale
            b   { boxX = floor (fromIntegral (boxX b) * scale) + boxX lBox
                , boxY = floor (fromIntegral (boxY b) * scale) + boxY lBox
                , boxWidth = floor $ fromIntegral (boxWidth b) * scale
                , boxHeight = floor $ fromIntegral (boxHeight b) * scale
                }
        )
        view

handleLayers :: Compositor -> Double -> Ptr WlrOutput
             -> [IORef [(View, SSDPrio, WlrBox)]] 
             -> PixmanRegion32 -> Way vs ws ()
handleLayers _ _ _ [] _ = pure ()
handleLayers comp secs output (l:ls) d = do
    handleLayers comp secs output ls d
    views <- liftIO $ readIORef l
    overs <- mapM (outputHandleView comp secs output d) views
    liftIO $ sequence_ overs

notifySurface :: Double -> Ptr WlrSurface -> IO ()
notifySurface secs surface = do
    callbacks <- surfaceGetCallbacks =<< getCurrentState surface
    forM_ callbacks $ \callback -> do
        cb <- callbackGetCallback callback
        callbackDone cb (floor $ secs * 1000)
        res <- callbackGetResource callback
        resourceDestroy res

    subs <- surfaceGetSubs surface
    forM_ subs $ \sub -> notifySurface secs =<< subSurfaceGetSurface sub


notifyView :: Double -> (View, SSDPrio, WlrBox) -> IO ()
notifyView secs (!view, _, _) = doJust (getViewSurface view) $ \surface -> do
    notifySurface secs surface
    renderViewAdditional (\s _ -> notifySurface secs s) view

notifyLayers :: Double -> [IORef [(View, SSDPrio, WlrBox)]] -> IO ()
notifyLayers _ [] = pure ()
notifyLayers secs (l:ls) = liftIO $ do
    notifyLayers secs ls
    views <- readIORef l
    mapM_ (notifyView secs) views

scissorOutput :: Ptr Renderer -> Ptr WlrOutput -> WlrBox -> IO ()
scissorOutput rend output !box = do
    Point w h <- outputTransformedResolution output
    trans <- getOutputTransform output
    let transform = composeOutputTransform
            outputTransformFlipped_180
            (invertOutputTransform trans)
    let transed = boxTransform box transform w h
    rendererScissor rend (Just transed)

frameHandler :: WSTag a => Double -> Output -> Way vs a ()
frameHandler secs out@Output {outputRoots = output, outputLayout = layers} = do
    enabled <- liftIO $ isOutputEnabled output
    needsSwap <- liftIO $ getOutputNeedsSwap output
    liftIO $ when enabled $ notifyLayers secs layers

    when (enabled && needsSwap) $ do
        comp <- wayCompositor <$> getState
        void . renderOn output (compRenderer comp) $ \age -> do
            let withDRegion = \act -> if age < 0 || age > 1
                then withRegion $ \region -> do
                        Point w h <- outputTransformedResolution output
                        resetRegion region . Just $ WlrBox 0 0 w h
                        act region
                else withRegionCopy (outputDamage out) $ \region -> do
                        let (b1, b2) = outputOldDamage out
                        pixmanRegionUnion region b1
                        pixmanRegionUnion region b2
                        pixmanRegionUnion region (getOutputDamage output)
                        act region
            renderBody <- makeCallback $ handleLayers comp secs output layers
            bg <- getEState
            clearFun <- liftIO $ case bg of
                    Nothing -> pure $ rendererClear (compRenderer comp) $ Color 0.25 0.25 0.25 1
                    Just (BackgroundTexture t bw bh) -> do
                        Point ow oh <- outputTransformedResolution output
                        let WlrBox x y _ _ = centerBox (WlrBox 0 0 bw bh) (WlrBox 0 0 ow oh)

                        pure $ do
                            rendererClear (compRenderer comp) $ Color 0.25 0.25 0.25 1
                            withMatrix $ \matrix -> do
                                getMatrix t matrix (getTransMatrix output) x y
                                renderWithMatrix (compRenderer comp) t matrix

            liftIO $ withDRegion $ \region -> do
                notEmpty <- pixmanRegionNotEmpty region
                when notEmpty $ do
                    boxes <- pixmanRegionBoxes region

                    forM_ boxes $ \box -> do
                        scissorOutput (compRenderer comp) output $ boxToWlrBox box
                        clearFun

                    renderBody region

                    rendererScissor (compRenderer comp) Nothing
                    let (b1, b2) = outputOldDamage out
                    copyRegion b1 b2
                    copyRegion b2 $ outputDamage out
                    pixmanRegionUnion b2 (getOutputDamage output)
                    resetRegion (outputDamage out) Nothing

fieteHandler :: WSTag a => Double -> Output -> Way vs a ()
fieteHandler secs Output {outputRoots = output, outputLayout = layers} = do
    enabled <- liftIO $ isOutputEnabled output
    needsSwap <- liftIO $ getOutputNeedsSwap output
    liftIO $ when enabled $ notifyLayers secs layers
    when (enabled && needsSwap) $ do
        comp <- wayCompositor <$> getState
        void . renderOn output (compRenderer comp) $ \_ -> do
            let withDRegion act = withRegion $ \region -> do
                        Point w h <- outputTransformedResolution output
                        resetRegion region . Just $ WlrBox 0 0 w h
                        scissorOutput (compRenderer comp) output $ WlrBox 0 0 w h
                        act region

            renderBody <- makeCallback $ handleLayers comp secs output layers
            liftIO $ withDRegion $ \region -> do
                rendererClear (compRenderer comp) $ Color 0.25 0.25 0.25 1
                renderBody region
