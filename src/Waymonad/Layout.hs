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
    , layoutOutput
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (writeIORef, readIORef)
import Data.Tuple (swap)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), centerBox)
import Graphics.Wayland.WlRoots.Output (getEffectiveBox, getOutputPosition)

import Waymonad (Way, WayBindingState (..), getState, WayLoggers (loggerLayout))
import Waymonad.Input.Seat (updatePointerFocus)
import Waymonad.Output.Core (getOutputId, outApplyDamage)
import Waymonad.Types (LogPriority(Debug), Output (..))
import Waymonad.Utility.Log (logPutText)
import Waymonad.Utility.Mapping (getOutputPointers)
import Waymonad.Utility.SSD
import Waymonad.View (setViewBox, viewHasCSD)
import Waymonad.ViewSet (WSTag (..), FocusCore (..))

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T


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


-- | update the layout cache for the given workspace.
reLayout :: forall vs a. (WSTag a, FocusCore vs a)
         => a -> Way vs a ()
reLayout ws = do
    state <- getState
    wstate <- liftIO . readIORef . wayBindingState $ state

    boxes <- getLayoutBoxes ws
    forM_ boxes $ \(out, _) ->
        outApplyDamage out Nothing

    forM_ boxes $ \(out, box) -> do
        let layout = getLayouted wstate ws box
        Point ox oy <- liftIO $ getOutputPosition $ outputRoots out

        let cacheRef = (M.!) (outputLayers out) "main"
        liftIO $ writeIORef cacheRef layout

        forM_  layout $ \(v, prio, b) -> do
            hasCSD <- viewHasCSD v
            let WlrBox bx by w h = getDecoBox hasCSD prio b
            setViewBox v (WlrBox (bx + ox) (by + oy) w h)
        logPutText loggerLayout Debug $
            "Set the layout for "
            `T.append` getName ws
            `T.append` "  on "
            `T.append` outputName out
            `T.append` " to: "
            `T.append` T.pack (show layout)

        pointers <- getOutputPointers out
        mapM_  updatePointerFocus pointers


layoutOutput :: (FocusCore vs ws, WSTag ws) => Output -> Way vs ws ()
layoutOutput output = do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    let ws = (\out -> IM.lookup (getOutputId out) . IM.fromList $ map swap $ (fmap . fmap) getOutputId mapping) $ output
    case ws of
        Just x -> reLayout x
        Nothing -> pure ()
