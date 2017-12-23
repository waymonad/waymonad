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
    , getCurrentWS
    , getCurrentView
    , getCurrentBox

    , withCurrentWS
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, listToMaybe)
import Data.IORef (readIORef)
import Data.Tuple (swap)

import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Output (getOutputBox)

import Input.Seat (Seat, getKeyboardFocus)
import {-# SOURCE #-} Output (Output (..), getOutputId)
import Utility (intToPtr)
import View (View)
import ViewSet (WSTag, Workspace)
import Waymonad
    ( Way
    , getViewSet
    , getSeat
    , getState
    , WayBindingState (..)
    )

import qualified Data.Map as M
import qualified Data.IntMap as IM

getPointerOutput :: Way a Output
getPointerOutput = do
    state <- getState
    (Just seat) <- getSeat
    currents <- liftIO . readIORef $ wayBindingCurrent state
    let (Just current) = M.lookup seat $ M.fromList currents
    pure . fst $ current

getCurrentOutput :: Way a (Maybe Output)
getCurrentOutput = do
    state <- getState
    (Just seat) <- getSeat
    currents <- liftIO . readIORef $ wayBindingCurrent state
    case M.lookup seat $ M.fromList currents of
        Just current -> pure . Just . snd $ current
        Nothing -> liftIO $ do
            outs <- readIORef $ wayBindingOutputs state
            pure $ listToMaybe outs

getCurrentWS :: (WSTag a) => Way a a
getCurrentWS = do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    output <- getCurrentOutput
    let ws = (\out -> IM.lookup (getOutputId out) . IM.fromList $ map swap $ (fmap . fmap) getOutputId mapping) =<< output
    case ws of
        Just x -> pure x
-- TODO: Make this a better error message!
        Nothing -> head . wayUserWorkspaces <$> getState

withCurrentWS
    :: (WSTag a)
    => (Seat -> Workspace -> b)
    -> Way a (Maybe b)
withCurrentWS fun = do
    seatM <- getSeat
    case seatM of
        Just seat -> do
            ws <- getCurrentWS
            vs <- getViewSet

            pure . Just . fun seat . fromJust $  M.lookup ws vs
        Nothing -> pure Nothing

getCurrentView :: WSTag a => Way a (Maybe View)
getCurrentView =
    getKeyboardFocus . fromJust =<< getSeat

getCurrentBox
    :: Way a (WlrBox)
getCurrentBox = do
    liftIO . getOutputBox . outputRoots =<< getPointerOutput

