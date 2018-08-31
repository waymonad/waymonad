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
import Graphics.Wayland.Server (callbackDone, OutputTransform (..))

import Graphics.Pixman
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), boxTransform, scaleBox)
import Graphics.Wayland.WlRoots.Backend (backendGetRenderer)
import Graphics.Wayland.WlRoots.Output
    ( WlrOutput, getOutputDamage , isOutputEnabled, getOutputNeedsSwap
    , invertOutputTransform, getOutputTransform
    , outputTransformedResolution, getTransMatrix
    , swapOutputBuffers, getOutputScale, makeOutputCurrent, outputGetBackend
    )
import Graphics.Wayland.WlRoots.Render
    ( Renderer, Texture, rendererScissor, rendererClear, renderWithMatrix
    , doRender, getTextureSize
    )
import Graphics.Wayland.WlRoots.Render.Color (Color (..))
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixProjectBox)
import Graphics.Wayland.WlRoots.Surface
    ( WlrSurface, subSurfaceGetSurface, surfaceGetSubs, subSurfaceGetBox
    , surfaceGetCallbacks, getCurrentState, callbackGetCallback
    , callbackGetResource, surfaceGetTexture, surfaceGetTransform
    , surfaceGetScale
    )

import Waymonad (makeCallback , unliftWay)
import Waymonad.Types (Way, Output (..), SSDPrio)
import Waymonad.Types.Core (View, SurfaceBuffer (..), ViewBuffer (..))
import Waymonad.View
    ( viewHasCSD, viewGetLocal, getViewSurface, renderViewAdditional
    , viewGetScale, getViewGeometry, viewGetPreserved
    )
import Waymonad.Utility.SSD (renderDeco, getDecoBox)
import Waymonad.Utility.Base (doJust)
import Waymonad.ViewSet (WSTag)

import qualified Graphics.Wayland.WlRoots.Buffer as B

renderOn :: Integer -> Ptr WlrOutput -> Ptr Renderer -> (Int -> Way vs ws a) -> Way vs ws (Maybe a)
renderOn time output rend act = doJust (liftIO $ makeOutputCurrent output) $ \age -> do
    ret <- liftIO . doRender rend output =<< unliftWay (act age)
    void . liftIO $ swapOutputBuffers output (Just time) Nothing
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


-- | Take a surface an render it onto the output frame buffer
outputHandleTexture
    :: Ptr WlrOutput -- ^ The output to render to
    -> PixmanRegion32 -- ^ The accumulated damage region on the output
    -> Ptr Texture -- ^ The texture to render onto the output
    -> OutputTransform -- ^ The Transformation value of the texture
    -> Float -- ^ The scale factor of the surface (for scaling geometry)
    -> WlrBox -- ^ The rectangle on the output to render into
    -> WlrBox -- ^ The rectangle on the texture to render
    -> IO ()
outputHandleTexture output damage texture txtTrans surfScale baseBox@(WlrBox !bx !by _ _) (WlrBox !geoX !geoY _ _) = do
    renderer <- backendGetRenderer =<< outputGetBackend output
    outputScale <- getOutputScale output
    (tW, tH) <- liftIO $ getTextureSize texture
    let realX = fromIntegral bx * outputScale - fromIntegral geoX * surfScale
        realY = fromIntegral by * outputScale - fromIntegral geoY * surfScale
        surfBox = WlrBox
            (floor $ realX)
            (floor $ realY)
            (ceiling $ fromIntegral tW * surfScale)
            (ceiling $ fromIntegral tH * surfScale)


    withMatrix $ \mat -> do
        let surfTransform = invertOutputTransform txtTrans
        matrixProjectBox mat surfBox surfTransform 0 (getTransMatrix output)
        renderDamaged renderer output damage baseBox $
            renderWithMatrix renderer texture mat


outputHandleSurface :: Double -> Ptr WlrOutput -> PixmanRegion32 -> Ptr WlrSurface -> Float -> WlrBox -> WlrBox -> IO ()
outputHandleSurface secs output damage surface scaleFactor baseBox@(WlrBox !bx !by _ _) geo@(WlrBox !geoX !geoY _ _) = do
    doJust (surfaceGetTexture surface) $ \texture -> do
        trans <- surfaceGetTransform surface
        surfScale <- fromIntegral <$> surfaceGetScale surface
        outputScale <- getOutputScale output
        let factor = scaleFactor * outputScale / surfScale
        outputHandleTexture output damage texture trans factor baseBox geo
        let realX = bx - (floor $ fromIntegral geoX * factor)
            realY = by - (floor $ fromIntegral geoY * factor)

        subs <- surfaceGetSubs surface
        forM_ subs $ \sub -> do
            sbox <- subSurfaceGetBox sub
            subsurf <- subSurfaceGetSurface sub
            outputHandleSurface
                secs
                output
                damage
                subsurf
                scaleFactor
                sbox{ boxX = floor (fromIntegral (boxX sbox) * factor) + realX
                    , boxY = floor (fromIntegral (boxY sbox) * factor) + realY
                    , boxWidth = floor $ fromIntegral (boxWidth sbox) * factor
                    , boxHeight = floor $ fromIntegral (boxHeight sbox) * factor
                    }
                (WlrBox 0 0 (boxWidth sbox) (boxHeight sbox))

outputHandlePreserved :: Ptr WlrOutput -> PixmanRegion32 -> SurfaceBuffer -> Float -> WlrBox -> WlrBox -> IO ()
outputHandlePreserved output damage buffer scaleFactor baseBox@(WlrBox !bx !by _ _) geo@(WlrBox !geoX !geoY _ _) = do
    doJust (B.getTexture $ surfaceBufferBuffer buffer) $ \texture -> do
        let trans = surfaceBufferTrans buffer
        let surfScale = fromIntegral $ surfaceBufferScale buffer
        outputScale <- getOutputScale output
        let factor = scaleFactor * outputScale / surfScale
        outputHandleTexture output damage texture trans factor baseBox geo
        let realX = bx - (floor $ fromIntegral geoX * factor)
            realY = by - (floor $ fromIntegral geoY * factor)

        forM_ (surfaceBufferSubs buffer) $ \(sbox, sub) -> do
            outputHandlePreserved
                output
                damage
                sub
                scaleFactor
                sbox{ boxX = floor (fromIntegral (boxX sbox) * factor) + realX
                    , boxY = floor (fromIntegral (boxY sbox) * factor) + realY
                    , boxWidth = floor $ fromIntegral (boxWidth sbox) * factor
                    , boxHeight = floor $ fromIntegral (boxHeight sbox) * factor
                    }
                (WlrBox 0 0 (boxWidth sbox) (boxHeight sbox))


outputHandleView :: Double -> Ptr WlrOutput -> PixmanRegion32 -> (View, SSDPrio, WlrBox) -> Way vs ws (IO ())
outputHandleView secs output d (!view, !prio, !obox) = do
    hasCSD <- viewHasCSD view
    let box = getDecoBox hasCSD prio obox
    scale <- liftIO $ viewGetScale view
    local <- liftIO $ viewGetLocal view
    let lBox = local { boxX = boxX box + boxX local, boxY = boxY box + boxY local}

    decoCB <- unliftWay $ renderDeco hasCSD prio output obox box
    renderer <- liftIO (backendGetRenderer =<< outputGetBackend output)
    liftIO $ renderDamaged renderer output d obox decoCB

    geo <- getViewGeometry view
    preM <- viewGetPreserved view
    case preM of
      Nothing -> doJust (getViewSurface view) $ \surface -> do
          liftIO $ outputHandleSurface secs output d surface scale lBox  geo
          pure $ renderViewAdditional (\s b ->
              void $ outputHandleSurface
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
                  (WlrBox 0 0 (boxWidth b) (boxHeight b))
                  )
              view
      Just (ViewBuffer pre) -> do
            liftIO $ outputHandlePreserved output d pre scale lBox geo
            pure $ pure ()


handleLayers :: Double -> Ptr WlrOutput
             -> [IORef [(View, SSDPrio, WlrBox)]] 
             -> PixmanRegion32 -> Way vs ws ()
handleLayers _ _ [] _ = pure ()
handleLayers secs output (l:ls) d = do
    handleLayers secs output ls d
    views <- liftIO $ readIORef l
    overs <- mapM (outputHandleView secs output d) views
    liftIO $ sequence_ overs

notifySurface :: Double -> Ptr WlrSurface -> IO ()
notifySurface secs surface = do
    callbacks <- surfaceGetCallbacks $ getCurrentState surface
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
scissorOutput rend output box = do
    transform <- getOutputTransform output
    Point w h <- outputTransformedResolution output
    rendererScissor rend (Just $ boxTransform box (invertOutputTransform transform) w h)

frameHandler :: WSTag a => Double -> Output -> Way vs a ()
frameHandler secs out@Output {outputRoots = output, outputLayout = layers} = do
    enabled <- liftIO $ isOutputEnabled output
    needsSwap <- liftIO $ getOutputNeedsSwap output
    liftIO $ when enabled $ notifyLayers secs layers

    when (enabled && needsSwap) $ do
        renderer <- liftIO (backendGetRenderer =<< outputGetBackend (outputRoots out))
        void . renderOn (floor $ secs * 1e9) output renderer $ \age -> do
            let withDRegion = \act -> if age < 0 || age > 2
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
            renderBody <- makeCallback $ handleLayers secs output layers
            liftIO $ withDRegion $ \region -> do
                notEmpty <- pixmanRegionNotEmpty region
                when notEmpty $ do
                    boxes <- pixmanRegionBoxes region

                    forM_ boxes $ \box -> do
                        scissorOutput renderer output $ boxToWlrBox box
                        liftIO $ rendererClear renderer $ Color 0.25 0.25 0.25 1

                    renderBody region

                    rendererScissor renderer Nothing
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
        renderer <- liftIO (backendGetRenderer =<< outputGetBackend output)
        void . renderOn (floor $ secs * 1e9) output renderer $ \_ -> do
            let withDRegion act = withRegion $ \region -> do
                    Point w h <- outputTransformedResolution output
                    resetRegion region . Just $ WlrBox 0 0 w h
                    scissorOutput renderer output $ WlrBox 0 0 w h
                    act region

            renderBody <- makeCallback $ handleLayers secs output layers
            liftIO $ withDRegion $ \region -> do
                rendererClear renderer $ Color 0.25 0.25 0.25 1
                renderBody region
