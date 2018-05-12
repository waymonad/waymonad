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
module Waymonad.Input.Cursor.Type
where

import Data.Functor.Identity (Identity)
import Data.IORef (IORef)
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Signal (ListenerToken)
import Graphics.Wayland.WlRoots.Cursor (WlrCursor)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons (ButtonState)
import Graphics.Wayland.WlRoots.Input.Pointer (AxisSource, AxisOrientation)

data CursorMapping c = CursorMapping
    { cursorMappingButton    :: c (Cursor -> Word32 -> Word32 -> ButtonState -> IO ())
    , cursorMappingMotion    :: c (Cursor -> Word32 -> Ptr InputDevice -> Double -> Double -> IO ())
    , cursorMappingMotionAbs :: c (Cursor -> Word32 -> Ptr InputDevice -> Double -> Double -> IO ())
    , cursorMappingAxis      :: c (Cursor -> Word32 -> AxisSource -> AxisOrientation -> Double -> Int32 -> IO ())
    }

data Cursor = Cursor
    { cursorRoots :: Ptr WlrCursor
    , cursorTokens :: IORef [ListenerToken] -- ^ Should be immutable, but we don't have to overdo the unsafePerformIO readIORef, do we?
    , cursorOutput :: IORef Int
    , cursorMapping :: IORef (CursorMapping Identity)
    }
