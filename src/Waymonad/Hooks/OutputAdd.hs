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
module Waymonad.Hooks.OutputAdd
    ( outputAddHook
    )
where

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Traversable (for)
import Data.List ((\\))

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))

import Waymonad (Way, WayBindingState (..), getState)
import Waymonad.Output (Output, getOutputBox)
import Waymonad.Types (OutputEvent (..), EvtCause (SideEffect))
import Waymonad.Utility.Base (These (..))
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Focus (setOutputWorkspace)
import Waymonad.Utility.Mapping (setSeatOutput)
import Waymonad.Utility.Pointer (sendSeatTo)
import Waymonad.Utility.Timing
import Waymonad.ViewSet (WSTag, FocusCore)

attachFreeWS :: (FocusCore vs a, WSTag a) => Output -> Way vs a ()
attachFreeWS out = do
    taken <- fmap (map fst) <$> liftIO . readIORef . wayBindingMapping =<< getState
    wss <- wayUserWorkspaces <$> getState

    case wss \\ taken of
        (x:_) -> setOutputWorkspace x out
        [] -> pure ()

attachFreeSeats :: (FocusCore vs ws, WSTag ws) => Output -> Way vs ws ()
attachFreeSeats out = do
    state <- getState
    seats <- liftIO . readIORef . wayBindingSeats $ state
    mapped <- liftIO . readIORef . wayBindingCurrent $ state

    let free = seats \\ map fst mapped
    void . for free $ \seat -> do
        doJust (getOutputBox out) $ \(WlrBox x y w h) ->
            sendSeatTo (Point (x + w `div` 2) (y + h `div` 2)) seat
        setSeatOutput seat (These out out) SideEffect


outputAddHook :: (FocusCore vs a, WSTag a) => OutputEvent -> Way vs a ()
outputAddHook (OutputEvent out) = do
    time :: Word <- getSeconds <$> getBasedTime
    when (time < 300) $ do
        attachFreeWS out
        attachFreeSeats out
