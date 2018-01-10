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
module WayUtil.Current
    ( getPointerOutput
    , getCurrentOutput
    , getCurrentView
    , getCurrentBox
    , getCurrentWS
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, listToMaybe)
import Data.IORef (readIORef)
import Data.Tuple (swap)

import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Output (getOutputBox)

import Input.Seat (getKeyboardFocus)
import {-# SOURCE #-} Output (Output (..), getOutputId)
import Utility (doJust)
import View (View)
import ViewSet (WSTag)
import Waymonad
    ( Way
    , getViewSet
    , getSeat
    , getState
    , WayBindingState (..)
    )

import qualified Data.Map as M
import qualified Data.IntMap as IM

-- TODO: This should be a Maybe, we aren't guaranteed outputs
getPointerOutput :: Way vs a Output
getPointerOutput = do
    state <- getState
    (Just seat) <- getSeat
    currents <- liftIO . readIORef $ wayBindingCurrent state
    let (Just current) = M.lookup seat $ M.fromList currents
    pure . fst $ current

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
-- TODO: Make this a better error message!
        Nothing -> head . wayUserWorkspaces <$> getState


getCurrentView :: Way vs a (Maybe View)
getCurrentView = doJust getSeat getKeyboardFocus

getCurrentBox :: Way vs a WlrBox
getCurrentBox =
    liftIO . getOutputBox . outputRoots =<< getPointerOutput

