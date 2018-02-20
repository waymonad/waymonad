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
module Waymonad.Hooks.KeyboardFocus
    ( handleKeyboardSwitch
    , handlePointerSwitch
    , handleKeyboardPull
    , handlePointerPull
    )
where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (fromMaybe)

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))

import Waymonad.Input.Seat (keyboardClear, updatePointerFocus)
import Waymonad.Output (getOutputBox)
import Waymonad.Utility.Base (whenJust, doJust)
import Waymonad.ViewSet (WSTag, FocusCore (..), getFirst)
import Waymonad.Utility.Base (These (..))
import Waymonad.Utility.Focus
import Waymonad.Utility.Pointer
import Waymonad.Utility.Layout (getViewPosition)
import Waymonad.Utility.Mapping (getOutputPointers, getOutputWS, setSeatOutput)
import Waymonad.Utility.ViewSet (unsetFocus, setFocused, withViewSet, getFocused)
import Waymonad
import Waymonad.Types

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

-- IfCxt?
handleKeyboardSwitch :: (FocusCore vs ws, WSTag ws) => SeatWSChange ws -> Way vs ws ()
handleKeyboardSwitch (SeatWSChange SeatKeyboard _ s pre cur) = do
    whenJust pre $ unsetFocus s
    case cur of
        -- We focused the void (in some way) clear focus
        Nothing -> keyboardClear s
        -- Focused something
        Just ws -> withViewSet (\_ vs -> _getFocused vs ws (Just s)) >>= \case
            -- Ok, we have a focused view here. Just do the usual focus
            -- setting procedure
            Just _ -> setFocused s SideEffect ws
            -- Nothing focused yet. Try master
            Nothing -> withViewSet (\_ vs -> getFirst vs ws) >>= \case
                -- Master doesn't exist. So this WS is empty, let's clear
                -- focus
                Nothing -> keyboardClear s
                -- Master exists, set as focused and start usual procedure
                Just v -> focusWSView v s ws
handleKeyboardSwitch _ = pure ()

handlePointerSwitch :: (FocusCore vs ws, WSTag ws) => OutputMappingEvent ws -> Way vs ws ()
handlePointerSwitch (OutputMappingEvent out _ _) = do
    pointers <- getOutputPointers out
    mapM_  updatePointerFocus pointers

handlePointerPull :: (FocusCore vs ws, WSTag ws)
                  => SeatOutputChange -> Way vs ws ()
handlePointerPull (SeatOutputChange SeatPointer Intentional s _ cur) = whenJust cur $ \out -> do
    setSeatOutput s (That out) SideEffect
handlePointerPull _ = pure ()


handleKeyboardPull :: (FocusCore vs ws, WSTag ws)
                   => SeatOutputChange -> Way vs ws ()
handleKeyboardPull (SeatOutputChange SeatKeyboard Intentional s _ cur) = whenJust cur $ \out -> do
    doJust (getOutputBox out) $ \ob@(WlrBox ox oy _ _) -> do
        box <- runMaybeT $ do
            ws <- MaybeT $ getOutputWS out
            view <- MaybeT $ getFocused s ws
            WlrBox vx vy vw vh <- MaybeT $ getViewPosition view out
            pure $ WlrBox (vx + ox) (vy + oy) vw vh
        let (WlrBox x y w h) = fromMaybe ob box
        sendSeatTo (Point (x + w `div` 2) (y + h `div` 2)) s
handleKeyboardPull _ = pure ()
