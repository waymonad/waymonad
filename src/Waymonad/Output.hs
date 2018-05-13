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
{-# LANGUAGE TupleSections #-}
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
module Waymonad.Output
    ( handleOutputAdd
    , handleOutputAdd'
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
    , outApplyDamage
    , setOutMode
    )
where

import Control.Exception (try)
import System.IO.Error (IOError)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.IORef (writeIORef, newIORef, readIORef, modifyIORef)
import Data.List ((\\), find)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))

import Graphics.Pixman
import Graphics.Wayland.Signal (removeListener)
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
    )

import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..))
import Graphics.Wayland.WlRoots.Output
    ( WlrOutput
    , OutputMode (..)
    , getModes
    , getOutputName
    , getOutputScale

    , setOutputMode
    , outputEnable
    , outputDisable
    , getWidth
    , getHeight
    , OutputSignals (..)
    , getOutputSignals
    , scheduleOutputFrame
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

import Waymonad (makeCallback2)
import Waymonad.Types (Compositor (..), WayHooks (..), OutputEvent (..), Output (..), OutputEffective (..))
import Waymonad.Utility.Signal
import Waymonad.Utility.Mapping (getOutputKeyboards, unsetSeatKeyboardOut)
import Waymonad.Input.Seat (Seat(seatLoadScale))
import Waymonad.Start (attachFrame)
import Waymonad.Utility.Base (doJust)
import Waymonad.Output.Render (frameHandler)
import Waymonad.ViewSet (WSTag (..), FocusCore)
import Waymonad
    ( Way
    , WayBindingState (..)
    , getState
    , getSeats
    )

import Waymonad.Output.Core

import qualified Data.Map.Strict as M

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

outputEffectiveChanged :: Output -> Way vs ws ()
outputEffectiveChanged out = do
    WayHooks {wayHooksOutputEffective = hook} <- wayCoreHooks <$> getState
    hook $ OutputEffective out

handleOutputAdd' :: (WSTag ws, FocusCore vs ws)
                 => (Double -> Output -> Way vs ws ())
                 -> (Output -> Way vs ws ())
                 -> Ptr WlrOutput
                 -> Way vs ws ()
handleOutputAdd' handler hook output = do
    name <- liftIO $ getOutputName output

    current <- wayBindingOutputs <$> getState
    active <- liftIO $ newIORef False

    let layerNames = ["overlay", "top", "override", "floating", "main", "bottom", "background"]
    layerPairs <- liftIO $ mapM (\x -> (x,) <$> newIORef []) layerNames
    let layers = map snd layerPairs
        layerMap = M.fromList layerPairs
    outputDamageR <- liftIO allocateRegion
    outputBuf1 <- liftIO allocateRegion
    outputBuf2 <- liftIO allocateRegion
    let out = Output output name active layers layerMap outputDamageR (outputBuf1, outputBuf2)
    liftIO $ modifyIORef current (out :)

    let signals = getOutputSignals output
    modeH <- setSignalHandler (outSignalMode signals) (const $ outputEffectiveChanged out)
    scaleH <- setSignalHandler (outSignalScale signals) (const $ outputEffectiveChanged out)
    transformH <- setSignalHandler (outSignalTransform signals) (const $ outputEffectiveChanged out)
    needsSwapH <- setSignalHandler (outSignalNeedsSwap signals) (const . liftIO $ scheduleOutputFrame (outputRoots out))

    hook out

    frameCB <- makeCallback2 (\t _ ->  handler t out)
    frameH <- liftIO $ attachFrame frameCB output

    setDestroyHandler (outSignalDestroy signals) (\dout -> do
        liftIO $ mapM_ removeListener [modeH, scaleH, transformH, needsSwapH, frameH]
        handleOutputRemove dout
                                                 )

handleOutputAdd :: (WSTag ws, FocusCore vs ws)
                => (Output -> Way vs ws ())
                -> Ptr WlrOutput
                -> Way vs ws ()
handleOutputAdd = handleOutputAdd' frameHandler

handleOutputRemove :: WSTag ws => Ptr WlrOutput -> Way vs ws ()
handleOutputRemove output = doJust (outputFromWlr output) $ \out -> do
    state <- getState
    removeOutputFromWork out
    liftIO $ modifyIORef (wayBindingOutputs state) $  \xs -> xs \\ [out]

removeOutputFromWork :: WSTag ws => Output -> Way vs ws ()
removeOutputFromWork output = do
    state <- getState
    let Compositor {compLayout = layout} = wayCompositor state

    liftIO $ removeOutput layout $ outputRoots output
    liftIO $ outputDisable (outputRoots output)
    liftIO $ modifyIORef (wayBindingMapping state) $ filter ((/=) output . snd)
    liftIO $ writeIORef (outputActive output) False

    keyboards <- getOutputKeyboards output
    mapM_ unsetSeatKeyboardOut keyboards


outputFromWlr :: Ptr WlrOutput -> Way vs a (Maybe Output)
outputFromWlr ptr = do
    outs <- liftIO . readIORef . wayBindingOutputs =<< getState
    pure . find ((==) ptr . outputRoots) $ outs

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

setOutMode :: MonadIO m => Ptr WlrOutput -> Ptr OutputMode -> m () -> m ()
setOutMode output mode cont = do
    ret :: Either IOError () <- liftIO $ try $ setOutputMode mode output
    case ret of
        Left _ -> pure ()
        Right _ -> cont

setPreferdMode :: MonadIO m => Ptr WlrOutput -> m () -> m ()
setPreferdMode output cont = do
    modes <- liftIO $ getModes output
    case modes of
        [] -> cont
        _ -> setOutMode output (last modes) cont

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
