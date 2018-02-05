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
    ( mappingChangeEvt
    , outputChangeEvt
    , wsChangeLogHook
    )
where

import Control.Monad (when, join, forM_)

import Waymonad.Output (Output)
import ViewSet (WSTag)
import Waymonad (getState)
import Waymonad.Types
import WayUtil
import WayUtil.Mapping (getOutputKeyboards, getOutputPointers)
import WayUtil.Log

checkOutput
    :: WSTag a
    => Maybe Output
    -> Maybe Output
    -> (Maybe a -> Maybe a -> SeatWSChange a)
    -> Way vs a ()
checkOutput pre cur con = do
    preWS <- join <$> traverse getOutputWS pre
    curWS <- join <$> traverse getOutputWS cur
    when (preWS /= curWS) $ do
        hook <- wayHooksSeatWSChange . wayCoreHooks <$> getState
        hook $ con preWS curWS

outputChangeEvt :: WSTag a => SeatOutputChange -> Way vs a ()
outputChangeEvt (PointerOutputChange seat pre cur) =
    checkOutput pre cur $ PointerWSChange seat
outputChangeEvt (KeyboardOutputChange seat pre cur) =
    checkOutput pre cur $ KeyboardWSChange seat

mappingChangeEvt :: WSTag a => OutputMappingEvent a -> Way vs a ()
mappingChangeEvt (OutputMappingEvent out pre cur) = do
    keys   <- getOutputKeyboards out
    points <- getOutputPointers  out

    hook <- wayHooksSeatWSChange . wayCoreHooks <$> getState
    forM_ points $ \point -> hook $ PointerWSChange point pre cur
    forM_ keys $ \key -> hook $ KeyboardWSChange key pre cur

wsChangeLogHook :: forall ws vs. WSTag ws => SeatWSChange ws -> Way vs ws ()
wsChangeLogHook evt = logPrint loggerWS Debug evt
