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
module Waymonad.Output.DamageDisplay
    ( damageDisplay
    )
where

import Control.Monad (forM_, when, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, newIORef, writeIORef, modifyIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Wayland.WlRoots.Render ( rendererScissor, rendererClear, renderColoredQuad)
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixProjectBox)
import Graphics.Wayland.WlRoots.Render.Color (Color (..))
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
import Graphics.Wayland.WlRoots.Output
    ( getOutputDamage , isOutputEnabled, getOutputNeedsSwap , outputTransformedResolution
    , setOutputNeedsSwap, getOutputTransform, getTransMatrix
    )
import Graphics.Pixman


import Waymonad (getState, makeCallback)
import Waymonad.Types ( Way, Output (..), Compositor (..) , WayBindingState (..))
import Waymonad.Utility.Base (ptrToInt)
import Waymonad.ViewSet (WSTag)

import Waymonad.Output.Render
import Debug.Trace 

import qualified Data.IntMap.Strict as IM

data DamageTracker = DamageTracker
    { damageRegions :: [PixmanRegion32]
    , damageIndex   :: IORef Int
    , damageSize    :: Int
    }

-- Global damage display map
damageMap :: IORef (IntMap DamageTracker)
{-# NOINLINE damageMap #-}
damageMap = unsafePerformIO $ newIORef mempty

getMap :: IO (IntMap DamageTracker)
getMap = readIORef damageMap

getOutputTracker :: Int -> Output -> IO DamageTracker
getOutputTracker size Output {outputRoots = roots} = do
    dMap <- getMap
    case IM.lookup (ptrToInt roots) dMap of
        Just x -> pure x
        Nothing -> do
            regions <- replicateM (size + 1) allocateRegion
            index <- newIORef 0
            let ret = DamageTracker regions index size
            modifyIORef damageMap $ IM.insert (ptrToInt roots) ret
            pure $ ret

setOutputDamage :: DamageTracker -> PixmanRegion32 -> IO ()
setOutputDamage tracker region = do
    index <- readIORef $ damageIndex tracker
    let target = damageRegions tracker !! index
    mapM_ (flip pixmanRegionSubtract region) $ damageRegions tracker
    copyRegion target region
    writeIORef (damageIndex tracker) $ (index + 1) `mod` (damageSize tracker + 1)

getTrackerRegions :: DamageTracker -> IO [(Float, PixmanRegion32)]
getTrackerRegions DamageTracker {damageRegions = regions, damageIndex = indexRef, damageSize = size} = do
    index <- readIORef indexRef
    let ret = take (size + 1) $ drop index $ cycle regions
        factor = 1 / (fromIntegral size + 1)
        added = fmap (* factor) [0 ..]
    pure $ zip added ret



damageDisplay :: WSTag ws => Int -> Double -> Output -> Way vs ws ()
damageDisplay depth secs out@Output {outputRoots = output, outputLayout = layers} = do
    enabled <- liftIO $ isOutputEnabled output
    needsSwap <- liftIO $ getOutputNeedsSwap output
    when (enabled && needsSwap) $ do
        comp <- wayCompositor <$> getState
        reEnable <- renderOn output (compRenderer comp) $ \age -> do
            tracker <- liftIO $ getOutputTracker depth out
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
                        mapM_ (pixmanRegionUnion region) $ damageRegions tracker
                        act region
            renderBody <- makeCallback $ handleLayers comp secs output layers
            liftIO $ withDRegion $ \region -> do
                notEmpty <- pixmanRegionNotEmpty region
                when notEmpty $ do
                    boxes <- pixmanRegionBoxes region
                    forM_ boxes $ \box -> do
                        scissorOutput (compRenderer comp) output $ boxToWlrBox box
                        liftIO $ rendererClear (compRenderer comp) $ Color 0.25 0.25 0.25 1

                    renderBody region

                    withRegion $ \dRegion -> do
                        pixmanRegionUnion dRegion (getOutputDamage output)
                        pixmanRegionUnion dRegion (outputDamage out)
                        setOutputDamage tracker dRegion

                    damages <- getTrackerRegions tracker
                    Point w h <- outputTransformedResolution output
                    let oBox = WlrBox 0 0 w h
                    transform <- getOutputTransform output
                    withMatrix $ \mat -> forM_ damages $ \(intensity, damage) -> do
                        let color = Color 1 0 0 intensity
                        dBoxes <- pixmanRegionBoxes damage
                        forM_ (traceShowId dBoxes) $ \dBox -> do
                            scissorOutput (compRenderer comp) output $ boxToWlrBox dBox
                            matrixProjectBox mat oBox transform 0 $ getTransMatrix output
                            renderColoredQuad (compRenderer comp) color mat


                    rendererScissor (compRenderer comp) Nothing
                    let (b1, b2) = outputOldDamage out
                    copyRegion b1 b2
                    copyRegion b2 $ outputDamage out
                    pixmanRegionUnion b2 (getOutputDamage output)
                    resetRegion (outputDamage out) Nothing
                pure notEmpty
        when (fromMaybe True reEnable) $ liftIO $ setOutputNeedsSwap output True
