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
module Hooks.SeatMapping
    ( SeatWSChangeEvent (..)
    , wsChangeEvtHook
    , wsChangeLogHook
    )
where

import Control.Monad (when, join, forM_)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Output (WlrOutput)

import Input.Seat (Seat)
import Output (Output)
import Utility (ptrToInt, whenJust)
import ViewSet (WSTag)
import Waymonad
import WayUtil
import WayUtil.Log
import WayUtil.Focus

data SeatWSChangeEvent a
    = PointerWSChangeEvent
        { seatWSChangeSeat :: Seat
        , seatWSChangePre :: Maybe a
        , seatWSChangeCur :: Maybe a
        }
    | KeyboardWSChangeEvent
        { seatWSChangeSeat :: Seat
        , seatWSChangePre :: Maybe a
        , seatWSChangeCur :: Maybe a
        } deriving (Eq, Show)

instance Typeable a => EventClass (SeatWSChangeEvent a)


checkOutput
    :: WSTag a
    => Maybe Output
    -> Maybe Output
    -> (Maybe a -> Maybe a -> SeatWSChangeEvent a)
    -> Way a ()
checkOutput pre cur con = do
    preWS <- fmap join $ traverse getOutputWS pre
    curWS <- fmap join $ traverse getOutputWS cur
    when (preWS /= curWS) $ sendEvent $ con preWS curWS

outputChangeEvt :: WSTag a => Maybe SeatOutputChangeEvent -> Way a ()
outputChangeEvt Nothing = pure ()
outputChangeEvt (Just (PointerOutputChangeEvent seat pre cur)) = checkOutput pre cur $ PointerWSChangeEvent seat
outputChangeEvt (Just (KeyboardOutputChangeEvent seat pre cur)) = checkOutput pre cur $ KeyboardWSChangeEvent seat

mappingChangeEvt :: WSTag a => Maybe (OutputMappingEvent a) -> Way a ()
mappingChangeEvt Nothing = pure ()
mappingChangeEvt (Just (OutputMappingEvent out pre cur)) = do
    keys   <- getOutputKeyboards out
    points <- getOutputPointers  out

    forM_ points $ \point -> sendEvent $ PointerWSChangeEvent point pre cur
    forM_ keys $ \key -> sendEvent $ KeyboardWSChangeEvent key pre cur

wsChangeEvtHook :: WSTag a => SomeEvent -> Way a ()
wsChangeEvtHook e =
       outputChangeEvt (getEvent e)
    <> mappingChangeEvt (getEvent e)

wsChangeLogHook :: forall a. WSTag a => SomeEvent -> Way a ()
wsChangeLogHook e = whenJust (getEvent e) $ \(evt :: SeatWSChangeEvent a) ->
    logPrint loggerWS Debug evt
