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
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Output
Description : The output type for Waymonad
Maintainer  : ongy
Stability   : testing
Portability : Linux

This is the core for outputs. Most modules will probably not use this,
unless they want to do something with the lifecycle of an output, rather than
workspace or layout management.

With that said, the intended lifecycle of an output:

1. An output is plugged in/created
2. The output is configured
3. The output is added to the layout/workarea
4. The output can be configured over IPC
5. The output is removed from the layout/workarea
6. The output disappears

The 5th step here may actually be caused by the 6th step.
Unplugging or destroying the output will trigger the transition from 4 over 5 to 6.

Steps 1 and 2 should be handled immediatly.
Adding the output to the (Waymoand) globals, to expose it over IPC, happens automatically.
Configuration of the output should be handled by the user. The hook in the main entry is
@wayUserConfOutputAdd@ in 'Waymonad.Main.WayUserConf'.

This now set up 'Output' should then be added to the workarea. This will then emit the core hook
@wayHooksSeatNewOutput@ in 'Waymonad.Types.WayHooks' which is responsible for setting up workspace
mappings or similar shenanigans.

At this point the output is set up and usable by the user and applications.

It can still be configured over IPC, though currently no events for this exist.

When it's unplugged the core will clean up any traces of it.
There is currently no core event for it, though that may change.
-}
module Output
    ( handleOutputAdd
    , handleOutputRemove
    , Output (..)
    , getOutputId
    , outputFromWlr
    , findMode
    , setOutputDirty
    , forOutput
    , readTransform
    , setPreferdMode
    , addOutputToWork
    , removeOutputFromWork
    , getOutputBox
    , intersectsOutput
    )
where

import Control.Exception (bracket_)
import Control.Monad (forM_, forM, filterM, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.IORef (IORef, writeIORef, newIORef, readIORef, modifyIORef)
import Data.List ((\\), find)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, ptrToIntPtr)
import Foreign.Storable (Storable(peek))

import Graphics.Wayland.Signal
import Graphics.Wayland.Server
    ( OutputTransform
    , outputTransformNormal
    , outputTransform180
    , outputTransform90
    , outputTransform270
    , outputTransformFlipped
    , outputTransformFlipped_180
    , outputTransformFlipped_90
    , outputTransformFlipped_270
    , callbackDone
    )

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..))
import Graphics.Wayland.WlRoots.Output
    ( WlrOutput
    , OutputMode (..)
    , getModes
    , getTransMatrix
    , swapOutputBuffers
    , makeOutputCurrent
    , getOutputName
    , getOutputScale

--    , getOutputNeedsSwap
    , setOutputNeedsSwap
    , isOutputEnabled
    , setOutputMode
    , outputEnable
    , outputDisable
    , getWidth
    , getHeight
    , OutputSignals (..)
    , getOutputSignals
    )
import Graphics.Wayland.WlRoots.OutputLayout
    ( outputIntersects
-- TODO: I think wlroots made this simpler
    , layoutOuputGetPosition
    , layoutGetOutput
    , addOutput
    , addOutputAuto
    , removeOutput
    )
import Graphics.Wayland.WlRoots.Render
    ( Renderer
    , renderWithMatrix
    , isTextureValid
    , doRender
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
    , surfaceGetScale
    --, surfaceHasDamage
    )

import Layout (layoutOutput)
import Waymonad (makeCallback2)
import Waymonad.Types (Compositor (..), WayHooks (..), OutputEvent (..), SSDPrio (..), Output (..))
import WayUtil.Signal
import Input.Seat (Seat(seatLoadScale))
import Shared (FrameHandler)
import Utility (doJust)
import View
    ( View
    , getViewSurface
    , renderViewAdditional
    , getViewBox
    , viewGetScale
    , viewGetLocal
--    , viewIsDirty
    , viewSetClean
    )
import ViewSet (WSTag (..), FocusCore)
import Waymonad
    ( Way
    , WayBindingState (..)
    , getState
    , getSeats
    )
import WayUtil.SSD

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text as T

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

renderOn :: Ptr WlrOutput -> Ptr Renderer -> IO a -> IO a
renderOn output rend act = bracket_
    (makeOutputCurrent output)
    (swapOutputBuffers output)
    (doRender rend output act)

outputHandleSurface :: Compositor -> Double -> Ptr WlrOutput -> Ptr WlrSurface -> Float -> WlrBox -> IO ()
outputHandleSurface comp secs output surface scaleFactor box = do
    outputScale <- getOutputScale output
    surfScale <- fromIntegral <$> surfaceGetScale surface
    let localScale = (outputScale / surfScale) * scaleFactor
    texture <- surfaceGetTexture surface
    isValid <- isTextureValid texture
    when isValid $ withMatrix $ \trans -> withMatrix $ \scale -> withMatrix $ \final -> do
            let x = fromIntegral (boxX box) * outputScale
                y = fromIntegral (boxY box) * outputScale

            matrixTranslate trans x y 0
            matrixScale scale localScale localScale 1
            matrixMul trans scale final
            withSurfaceMatrix surface (getTransMatrix output) final $ \mat ->
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
                    sbox{ boxX = floor (fromIntegral (boxX sbox) * scaleFactor * outputScale) + boxX box
                        , boxY = floor (fromIntegral (boxY sbox) * scaleFactor * outputScale) + boxY box
                        , boxWidth = floor $ fromIntegral (boxWidth sbox) * scaleFactor
                        , boxHeight = floor $ fromIntegral (boxHeight sbox) * scaleFactor
                        }

outputHandleView :: Compositor -> Double -> Ptr WlrOutput -> (View, SSDPrio, WlrBox) -> IO (IO ())
outputHandleView comp secs output (view, prio, obox) = doJust (getViewSurface view) $ \surface -> do
    let box = getDecoBox True prio obox
    viewSetClean view
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

handleLayers :: Compositor -> Double -> Ptr WlrOutput -> [IORef [(View, SSDPrio, WlrBox)]] -> IO ()
handleLayers _ _ _ [] = pure ()
handleLayers comp secs output (l:ls) = do
    handleLayers comp secs output ls
    views <- readIORef l
    overs <- mapM (outputHandleView comp secs output) views
    sequence_ overs

frameHandler :: WSTag a
             => Double -> Output -> Way vs a ()
frameHandler secs Output {outputRoots = output, outputLayout = layers} = do
    enabled <- liftIO $ isOutputEnabled output
    when enabled $ do
        comp <- wayCompositor <$> getState
        liftIO $ renderOn output (compRenderer comp) $
            handleLayers comp secs output layers

findMode :: MonadIO m
         => Ptr WlrOutput -> Word32
         -> Word32 -> Maybe Word32
         -> m (Maybe (Ptr OutputMode))
findMode output width height refresh = liftIO $ do
    modes <- getModes output
    paired <- forM modes $ \x -> do
        marshalled <- peek x
        pure (marshalled, x)
    let candidates = filter (\(mode, _) -> modeWidth mode == width && modeHeight mode == height) paired

    let fun = case refresh of
            Nothing -> maximumBy (compare  `on` (modeRefresh . fst))
            Just val -> minimumBy (compare `on` (abs . (-) (fromIntegral val :: Int) . fromIntegral . modeRefresh . fst))

    pure $ case candidates of
        [] -> Nothing
        xs -> Just . snd . fun $ xs

handleOutputAdd :: (WSTag ws, FocusCore vs ws)
                => (Output -> Way vs ws ())
                -> Ptr WlrOutput
                -> Way vs ws FrameHandler
handleOutputAdd hook output = do
    name <- liftIO $ getOutputName output

    current <- wayBindingOutputs <$> getState
    active <- liftIO $ newIORef False

    mainLayer <- liftIO $ newIORef []
    floatLayer <- liftIO $ newIORef []
    overrideLayer <- liftIO $ newIORef []
    let layers = [overrideLayer, floatLayer, mainLayer]
        layerMap = M.fromList [("override", overrideLayer), ("floating", floatLayer), ("main", mainLayer)]
    let out = Output output name active layers layerMap
    liftIO $ modifyIORef current (out :)

    let signals = getOutputSignals output
    modeH <- setSignalHandler (outSignalMode signals) (const $ layoutOutput out)
    scaleH <- setSignalHandler (outSignalScale signals) (const $ layoutOutput out)
    transformH <- setSignalHandler (outSignalTransform signals) (const $ layoutOutput out)

    setDestroyHandler (outSignalDestroy signals) (const $ liftIO $ mapM_ removeListener [modeH, scaleH, transformH])

    hook out
    makeCallback2 (\t _ ->  frameHandler t out)

handleOutputRemove
    :: Ptr WlrOutput
    -> Way vs a ()
handleOutputRemove output = doJust (outputFromWlr output) $ \out -> do
    state <- getState
    removeOutputFromWork out
    liftIO $ modifyIORef (wayBindingOutputs state) $  \xs -> xs \\ [out]

removeOutputFromWork :: Output -> Way vs ws ()
removeOutputFromWork output = do
    state <- getState
    let Compositor {compLayout = layout} = wayCompositor state
    liftIO $ removeOutput layout $ outputRoots output
    liftIO $ outputDisable (outputRoots output)
    liftIO $ modifyIORef (wayBindingMapping state) $ filter ((/=) output . snd)
    liftIO $ writeIORef (outputActive output) False

getOutputId :: Output -> Int
getOutputId = ptrToInt . outputRoots

outputFromWlr :: Ptr WlrOutput -> Way vs a (Maybe Output)
outputFromWlr ptr = do
    outs <- liftIO . readIORef . wayBindingOutputs =<< getState
    pure . find ((==) ptr . outputRoots) $ outs

setOutputDirty :: MonadIO m => Output -> m ()
setOutputDirty out = liftIO $ setOutputNeedsSwap (outputRoots out) True

forOutput :: (Output -> Way vs ws a) -> Way vs ws [a]
forOutput fun = do
    current <- wayBindingOutputs <$> getState
    outs <- liftIO $ readIORef current
    mapM fun outs

readTransform :: Text -> Maybe OutputTransform
readTransform "Normal" = Just outputTransformNormal
readTransform "90" = Just outputTransform90
readTransform "180" = Just outputTransform180
readTransform "270" = Just outputTransform270
readTransform "Flipped" = Just outputTransformFlipped
readTransform "Flipped90" = Just outputTransformFlipped_90
readTransform "Flipped180" = Just outputTransformFlipped_180
readTransform "Flipped270" = Just outputTransformFlipped_270
readTransform _ = Nothing

setPreferdMode :: MonadIO m => Ptr WlrOutput -> m ()
setPreferdMode output = liftIO $ do
    modes <- getModes output
    case modes of
        [] -> pure ()
        _ -> setOutputMode (last modes) output

addOutputToWork :: Output -> Maybe Point -> Way vs ws ()
addOutputToWork output position = do
    Compositor {compLayout = layout} <- wayCompositor <$> getState
    liftIO $ case position of
        Nothing -> addOutputAuto layout $ outputRoots output
        Just (Point x y) -> addOutput layout (outputRoots output) x y
    WayHooks {wayHooksNewOutput = hook} <- wayCoreHooks <$> getState

    scale <- liftIO $ getOutputScale (outputRoots output)
    seats <- getSeats
    liftIO $ forM_ seats $ \seat -> seatLoadScale seat scale
    liftIO $ writeIORef (outputActive output) True
    liftIO $ outputEnable (outputRoots output)

    hook (OutputEvent output)

getOutputBox :: Output -> Way vs ws (Maybe WlrBox)
getOutputBox Output { outputRoots = output } = do
    Compositor {compLayout = layout} <- wayCompositor <$> getState
    (Point ox oy) <- liftIO (layoutOuputGetPosition =<< layoutGetOutput layout output)
    width <- liftIO $ getWidth output
    height <- liftIO $ getHeight output
    pure $ Just $ WlrBox ox oy (fromIntegral width) (fromIntegral height)

intersectsOutput :: Output -> WlrBox -> Way vs ws Bool
intersectsOutput Output {outputRoots = out} box = do
    Compositor {compLayout = layout} <- wayCompositor <$> getState
    liftIO $ outputIntersects layout out box
