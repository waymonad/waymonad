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
module Hooks.KeyboardFocus
where

import Control.Monad (void)

import Hooks.SeatMapping
import Input.Seat (Seat, keyboardClear, keyboardEnter)
import Utility (whenJust)
import ViewSet (WSTag, getFocused, getMaster)
import WayUtil
import WayUtil.Focus
import WayUtil.Log
import WayUtil.ViewSet (unsetFocus, setFocused, withWS, modifyWS)
import Waymonad
import qualified ViewSet as VS

-- TODO: SANITIZE!!!!
{- So the general idea here:
 - If we focus an empty workspace -> clear focus
 - If we focus a populated workspace:
 - Check if we have a focused window, if yes -> focus
 - Otherwise, try to focus master, repeate ^
 - If still Nothing focused, clear focus
 -}
-- Do NOT asume this has the relevant seat as current seat.
-- This may be triggered by another seat on the same output changing the
-- output <-> seat mapping!
handleKeyboardSwitch :: WSTag a => SomeEvent -> Way a ()
handleKeyboardSwitch e = case getEvent e of
    Just (KeyboardWSChangeEvent s pre cur) -> do
        whenJust pre $ unsetFocus s
        case cur of
            Nothing -> void $ keyboardClear s
            Just ws -> (withWS ws $ getFocused s) >>= \case
                Just v -> void $ keyboardEnter s v
                Nothing -> (withWS ws getMaster) >>= \case
                    Nothing -> keyboardClear s
                    Just v -> do
                        modifyWS (VS.setFocused v s) ws
                        void $ keyboardEnter s v
    _ -> pure ()
