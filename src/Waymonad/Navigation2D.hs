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
{-# LANGUAGE OverloadedStrings #-}
module Waymonad.Navigation2D
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.List (lookup, sortOn)
import Data.Maybe (listToMaybe)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Waymonad.Utility.Base (doJust, whenJust)
import Waymonad.ViewSet
import Waymonad.Types (Output (outputLayers), Way)
import Waymonad.Utility.Current (getCurrentOutput, getCurrentView)
import Waymonad.Utility.Focus (focusView)

import qualified Data.Map.Strict as M

moveLeft :: (FocusCore vs ws, WSTag ws) => Way vs ws ()
moveLeft = doJust getCurrentOutput $ \output ->
    doJust getCurrentView $ \view -> do
        ws <- map (\(l, _, r) -> (l, r)) <$> (liftIO $ readIORef $ (M.!) (outputLayers output) "main")
        whenJust (lookup view ws) $ \(WlrBox cx cy _ ch) -> do
                let candidates = filter (\(_, WlrBox x _ w _) -> abs (x + w - cx) < 5) ws
                    metric (_, (WlrBox _ y _ h)) = min (cy + ch) (y + h) - max cy y
                    sorted = sortOn (negate . metric) candidates
                 in whenJust (listToMaybe sorted) $ focusView . fst

moveRight :: (FocusCore vs ws, WSTag ws) => Way vs ws ()
moveRight = doJust getCurrentOutput $ \output ->
    doJust getCurrentView $ \view -> do
        ws <- map (\(l, _, r) -> (l, r)) <$> (liftIO $ readIORef $ (M.!) (outputLayers output) "main")
        whenJust (lookup view ws) $ \(WlrBox cx cy cw ch) -> do
                let candidates = filter (\(_, WlrBox x _ _ _) -> abs (x - cx - cw) < 5) ws
                    metric (_, (WlrBox _ y _ h)) = min (cy + ch) (y + h) - max cy y
                    sorted = sortOn (negate . metric) candidates
                 in whenJust (listToMaybe sorted) $ focusView . fst

moveUp :: (FocusCore vs ws, WSTag ws) => Way vs ws ()
moveUp = doJust getCurrentOutput $ \output ->
    doJust getCurrentView $ \view -> do
        ws <- map (\(l, _, r) -> (l, r)) <$> (liftIO $ readIORef $ (M.!) (outputLayers output) "main")
        whenJust (lookup view ws) $ \(WlrBox cx cy cw _) -> do
                let candidates = filter (\(_, WlrBox _ y _ h) -> abs (y + h - cy) < 5) ws
                    metric (_, (WlrBox x _ w _)) = min (cx + cw) (x + w) - max cx x
                    sorted = sortOn (negate . metric) candidates
                 in whenJust (listToMaybe sorted) $ focusView . fst

moveDown :: (FocusCore vs ws, WSTag ws) => Way vs ws ()
moveDown = doJust getCurrentOutput $ \output ->
    doJust getCurrentView $ \view -> do
        ws <- map (\(l, _, r) -> (l, r)) <$> (liftIO $ readIORef $ (M.!) (outputLayers output) "main")
        whenJust (lookup view  ws) $ \(WlrBox cx cy cw ch) -> do
                let candidates = filter (\(_, WlrBox _ y _ _) -> abs (y - ch - cy) < 5) ws
                    metric (_, (WlrBox x _ w _)) = min (cx + cw) (x + w) - max cx x
                    sorted = sortOn (negate . metric) candidates
                 in whenJust (listToMaybe sorted) $ focusView . fst
