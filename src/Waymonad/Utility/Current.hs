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
module Waymonad.Utility.Current
    ( getPointerOutput
    , getPointerOutputS
    , getCurrentOutput
    , getCurrentView
    , getCurrentBox
    , getCurrentWS
    , getPointerWS
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.IORef (readIORef)
import Data.Tuple (swap)

import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Output (getOutputBox)

import {-# SOURCE #-} Waymonad.Input.Seat (Seat, getKeyboardFocus)
import Waymonad.Output.Core (getOutputId)
import Waymonad.Utility.Base (doJust)
import Waymonad.View (View)
import Waymonad
    ( Way
    , getSeat
    , getState
    , WayBindingState (..)
    )
import Waymonad.Types (Output (..))

import qualified Data.Map as M
import qualified Data.IntMap as IM

getPointerOutput :: Way vs a (Maybe Output)
getPointerOutput = doJust getSeat getPointerOutputS

getPointerOutputS :: Seat -> Way vs ws (Maybe Output)
getPointerOutputS seat = do
    state <- getState
    currents <- liftIO . readIORef $ wayBindingCurrent state
    let current = M.lookup seat $ M.fromList currents
    pure . fmap fst $ current

getCurrentOutput :: Way vs a (Maybe Output)
getCurrentOutput = doJust getSeat $ \seat -> do
    state <- getState
    currents <- liftIO . readIORef $ wayBindingCurrent state
    case M.lookup seat $ M.fromList currents of
        Just current -> pure . Just . snd $ current
        Nothing -> liftIO $ do
            outs <- readIORef $ wayBindingOutputs state
            pure $ listToMaybe outs

getCurrentWS :: Way vs a a
getCurrentWS = do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    output <- getCurrentOutput
    let ws = (\out -> IM.lookup (getOutputId out) . IM.fromList $ map swap $ (fmap . fmap) getOutputId mapping) =<< output
    case ws of
        Just x -> pure x
        Nothing -> fromMaybe (error "At least one workspace is requried") . listToMaybe . wayUserWorkspaces <$> getState

getPointerWS :: Way vs a a
getPointerWS = do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    output <- getPointerOutput
    let ws = (\out -> IM.lookup (getOutputId out) . IM.fromList $ map swap $ (fmap . fmap) getOutputId mapping) =<< output
    case ws of
        Just x -> pure x
        Nothing -> fromMaybe (error "At least one workspace is requried") . listToMaybe . wayUserWorkspaces <$> getState


getCurrentView :: Way vs a (Maybe View)
getCurrentView = doJust getSeat getKeyboardFocus

getCurrentBox :: Way vs a (Maybe WlrBox)
getCurrentBox =
    doJust getPointerOutput (fmap Just . liftIO . getOutputBox . outputRoots)

