{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2018  Markus Ongyerth

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
module Waymonad.Utility.Pointer
where

import Control.Monad.IO.Class (liftIO)

import Graphics.Wayland.WlRoots.Box (Point (..), WlrBox (..))
import Graphics.Wayland.WlRoots.OutputLayout (getOutputLayoutExtends)

import Waymonad.Utility.Base (doJust)
import Waymonad.ViewSet
import Waymonad (getSeat, getState)
import Waymonad.Input.Seat (setPointerPosition, Seat)
import Waymonad.Types


sendPointerTo :: (FocusCore vs ws, WSTag ws) => Point -> Way vs ws ()
sendPointerTo p = doJust getSeat $ sendSeatTo p

sendSeatTo :: (FocusCore vs ws, WSTag ws) => Point -> Seat -> Way vs ws ()
sendSeatTo (Point dx dy) seat = do
    Compositor {compLayout = layout} <- wayCompositor <$> getState
    WlrBox _ _ lw lh <- liftIO $ getOutputLayoutExtends layout
    let pos = (fromIntegral dx / fromIntegral lw, fromIntegral dy / fromIntegral lh)
    setPointerPosition seat pos
