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
module Waymonad.Hooks.FocusFollowPointer
where

import Waymonad.Types (SeatFocusChange (..), Way, EvtCause (..), SeatEvent (..))
import Waymonad.View (doFocusView)
import Waymonad.ViewSet (FocusCore, WSTag)

focusFollowPointer :: (WSTag ws, FocusCore vs ws) => SeatFocusChange -> Way vs ws ()
focusFollowPointer (SeatFocusChange SeatPointer Intentional seat _ (Just view)) =
    doFocusView view seat
focusFollowPointer _ = pure ()
