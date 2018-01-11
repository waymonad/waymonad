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
import Input.Seat (keyboardClear)
import Utility (whenJust)
import ViewSet (WSTag, FocusCore (..), getFirst)
import WayUtil.Focus
import WayUtil.ViewSet (unsetFocus, setFocused, withViewSet)
import Waymonad
import Waymonad.Types

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
handleKeyboardSwitch :: (FocusCore vs ws, WSTag ws) => SeatWSChange ws -> Way vs ws ()
handleKeyboardSwitch (KeyboardWSChange s pre cur) = do
    whenJust pre $ unsetFocus s
    case cur of
        -- We focused the void (in some way) clear focus
        Nothing -> void $ keyboardClear s
        -- Focused something
        Just ws -> withViewSet (\_ vs -> _getFocused vs ws (Just s)) >>= \case
            -- Ok, we have a focused view here. Just do the usual focus
            -- setting procedure
            Just _ -> setFocused s ws
            -- Nothing focused yet. Try master
            Nothing -> withViewSet (\_ vs -> getFirst vs ws) >>= \case
                -- Master doesn't exist. So this WS is empty, let's clear
                -- focus
                Nothing -> keyboardClear s
                -- Master exists, set as focused and start usual procedure
                Just v -> do
                    focusWSView v ws
                    setFocused s ws
handleKeyboardSwitch _ = pure ()
