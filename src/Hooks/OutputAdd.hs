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
{-# LANGUAGE ScopedTypeVariables #-}
module Hooks.OutputAdd
    ( outputAddHook
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.List ((\\))

import Waymonad.Output (Output)
import Utility (These (..))
import ViewSet (WSTag, FocusCore)
import WayUtil.Mapping (setSeatOutput)
import WayUtil.Focus (setOutputWorkspace)
import WayUtil.Timing
import Waymonad (Way, WayBindingState (..), getState)
import Waymonad.Types (OutputEvent (..))

attachFreeWS :: (FocusCore vs a, WSTag a) => Output -> Way vs a ()
attachFreeWS out = do
    taken <- fmap (map fst) <$> liftIO . readIORef . wayBindingMapping =<< getState
    wss <- wayUserWorkspaces <$> getState

    case wss \\ taken of
        (x:_) -> setOutputWorkspace x out
        [] -> pure ()

attachFreeSeats :: WSTag ws => Output -> Way vs ws ()
attachFreeSeats out = do
    state <- getState
    seats <- liftIO . readIORef . wayBindingSeats $ state
    mapped <- liftIO . readIORef . wayBindingCurrent $ state

    let free = seats \\ map fst mapped
    mapM_ (`setSeatOutput` These out out) free


outputAddHook :: (FocusCore vs a, WSTag a) => OutputEvent -> Way vs a ()
outputAddHook (OutputEvent out) = do
    time :: Word <- getSeconds <$> getBasedTime
    when (time < 300) $ do
        attachFreeWS out
        attachFreeSeats out
