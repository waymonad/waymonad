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
module Waymonad.Input
    ( Input (..)
    )
where

import Data.IORef (IORef)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.XCursorManager (WlrXCursorManager)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.Signal (ListenerToken)

import Waymonad.Input.Cursor.Type (Cursor)
import {-# SOURCE #-} Waymonad.Input.Seat (Seat)

data SeatFoo = SeatFoo
    { fooXCursorManager :: Ptr WlrXCursorManager
    , fooCursor :: Cursor
    , fooSeat :: Seat
    , fooImageToken :: ListenerToken
    }

data Input = Input
    { inputDevices :: IORef (Set (Ptr InputDevice))
    , inputFooMap :: IORef (Map Text SeatFoo)
    , inputAddToken :: [ListenerToken]
    }
