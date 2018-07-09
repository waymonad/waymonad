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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Waymonad.Layout
    ( reLayout
    , delayedLayout
    , layoutOutput
    , getWSLayout
    , freezeLayout
    , sendLayout
    , applyLayout
    )
where

import System.IO

import Control.Monad (forM_, forM, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (writeIORef, readIORef, newIORef, IORef, modifyIORef')
import Data.Tuple (swap)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), centerBox)
import Graphics.Wayland.WlRoots.Output (getEffectiveBox, getOutputPosition)

import Waymonad (Way, WayBindingState (..), getState, registerTimed, unliftWay)
import Waymonad.Input.Seat (updatePointerFocus)
import Waymonad.Output.Core (getOutputId, outApplyDamage)
import Waymonad.Types (Output (..), SSDPrio)
import Waymonad.Types.Core (View)
import Waymonad.Utility.Mapping (getOutputPointers)
import Waymonad.Utility.SSD
import Waymonad.View (setViewSize, updateViewSize, viewHasCSD, moveView, preserveTexture, dropTexture)
import Waymonad.ViewSet (WSTag (..), FocusCore (..))

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM


getBoxes
    :: WSTag a
    => a
    -> Way vs a [(Output, WlrBox)]
getBoxes ws = do
    xs <- liftIO . readIORef . wayBindingMapping =<< getState
    let outputs = map snd . filter ((==) ws . fst) $ xs
    liftIO $ mapM (\out -> fmap (out,) . getEffectiveBox $ outputRoots out) outputs


getLayoutBoxes
    :: WSTag a
    => a
    -> Way vs a [(Output, WlrBox)]
getLayoutBoxes ws = do
    outs <- getBoxes ws

    let smallest :: WlrBox = foldr (shrink . snd) (WlrBox 0 0 maxBound maxBound) outs
    pure $ map (fmap $ centerBox smallest . toOrigin) outs
    where   shrink :: WlrBox -> WlrBox -> WlrBox
            shrink (WlrBox _ _ lw lh) (WlrBox _ _ rw rh) = WlrBox 0 0 (min lw rw) (min lh rh)
            toOrigin :: WlrBox -> WlrBox
            toOrigin (WlrBox _ _ w h) = WlrBox 0 0 w h

getWSLayout :: forall vs ws . (WSTag ws, FocusCore vs ws) => vs -> ws -> Way vs ws [(Output, [(View, SSDPrio, WlrBox)])]
getWSLayout vs ws = do
    boxes <- getLayoutBoxes ws
    forM boxes $ \(out, box) -> do
        let layout = getLayouted vs ws box
        pure (out, layout)


freezeLayout :: (WSTag ws, FocusCore vs ws)
             => ws -> vs -> Way vs ws (IO ())
freezeLayout ws vs = do
    --state <- getState
    --vs <- liftIO . readIORef . wayBindingState $ state
    layouts <- getWSLayout vs ws
    case layouts of
        [] -> pure $ pure ()
        ((_, layout):_) -> do
            forM_ layout $ \(v, _, _) -> preserveTexture v
            pure $ forM_ layout $ \(v, _, _) -> dropTexture v


sendLayout :: (Int, Int) -- ^The output position, for moving Xwayland windows properly
           -> [(View, SSDPrio, WlrBox)] -- ^The layout
           -> IO () -- ^An action to be applied after all windows are ready (e.g. apply the cache)
           -> Way vs ws (Maybe (IO ())) -- ^If applicable, returns a function to cancel the timeout/trigger after ready
sendLayout (ox, oy) layout act = do
    updateRef :: IORef Int <- liftIO $ newIORef 0
    let checkedAct = do
            val <- readIORef updateRef
            when (val == 0) $ act

    forM_  layout $ \(v, prio, b) -> do
        hasCSD <- viewHasCSD v
        let WlrBox bx by w h = getDecoBox hasCSD prio b
        moveView v (bx + ox) (by + oy)
        wait <- setViewSize v w h (modifyIORef' updateRef (subtract 1) >> checkedAct)
        when wait $ liftIO $ do
            preserveTexture v
            modifyIORef' updateRef (+ 1)

    let force = liftIO $ do
            cur <- readIORef updateRef
            when (cur > 0) $ do
                hPutStrLn stderr "Timed out"
                writeIORef updateRef 0
                act

    suspended <- liftIO (readIORef updateRef)
    case suspended of
        0 -> liftIO act >> pure Nothing
        _ -> do
            -- @60Hz, 3frames = 50ms
            registerTimed force 51
            pure . Just $ writeIORef updateRef 0

applyLayout :: (WSTag ws, FocusCore vs ws) => Output -> [(View, SSDPrio, WlrBox)] -> Way vs ws ()
applyLayout out layout = do
    outApplyDamage out Nothing
    let cacheRef = (M.!) (outputLayers out) "main"
    liftIO $ writeIORef cacheRef layout

    forM_  layout $ \(v, prio, b) -> do
        hasCSD <- viewHasCSD v
        let WlrBox _ _ w h = getDecoBox hasCSD prio b
        updateViewSize v w h
        dropTexture v

    pointers <- getOutputPointers out
    mapM_ updatePointerFocus pointers

delayedLayout :: (WSTag ws, FocusCore vs ws) => ws -> Way vs ws ()
delayedLayout ws = do
    state <- getState
    vs <- liftIO . readIORef . wayBindingState $ state
    unfreeze <- freezeLayout ws vs
    post <- getWSLayout vs ws

    case post of
        ((out, layout):_) -> do
            Point ox oy <- liftIO $ getOutputPosition $ outputRoots out
            doApply <- unliftWay $ mapM_ (uncurry applyLayout) post
            void $ sendLayout (ox, oy) layout (unfreeze >> doApply)
        _ -> pure ()


-- | update the layout cache for the given workspace.
reLayout :: (WSTag ws, FocusCore vs ws)
         => ws -- ^The workspace to re-layout
--         -> IO () -- ^An IO action to clean up any resources bound by the current layout. This will be executed after the layout is updated. This can be delayed by further updates as well
         -> Way vs ws ()
reLayout ws = do
    state <- getState
    vs <- liftIO . readIORef . wayBindingState $ state
    layouts <- getWSLayout vs ws

    case layouts of
        [] -> pure () -- No need to do anything
        ((out, layout):_) -> do
            Point ox oy <- liftIO $ getOutputPosition $ outputRoots out
            _ <- sendLayout (ox, oy) layout (pure ())
            mapM_ (uncurry applyLayout) layouts

layoutOutput :: (FocusCore vs ws, WSTag ws) => Output -> Way vs ws ()
layoutOutput output = do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    let ws = (\out -> IM.lookup (getOutputId out) . IM.fromList $ map swap $ (fmap . fmap) getOutputId mapping) $ output
    case ws of
        Just x -> delayedLayout x
        Nothing -> pure ()
