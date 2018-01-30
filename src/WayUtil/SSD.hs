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
module WayUtil.SSD
where

import Graphics.Wayland.WlRoots.Box (WlrBox, Point)

import Waymonad.Types

-- data SSDPrio vs ws
--     = ForcedSSD (ServerSideDecoration vs ws)
--     | SuggestedSSD (ServerSideDecoration vs ws)
--     | NoSSD

-- data ServerSideDecoration vs ws = SSD
--       ssdGetPoint :: Point -> Maybe Point
--     , ssdGetBox   :: WlrBox -> WlrBox
--     , ssdDraw     :: WlrBox -> WlrBox -> Way vs ws ()

getDecoBox :: Bool -> SSDPrio -> WlrBox -> WlrBox
getDecoBox _    (ForcedSSD SSD {ssdGetBox = fun}) box = fun box
getDecoBox True (SuggestedSSD SSD {ssdGetBox = fun}) box = fun box
getDecoBox _    _ box = box


getDecoPoint :: Bool -> SSDPrio -> Point -> Point
getDecoPoint _    (ForcedSSD SSD {ssdGetPoint = fun}) p = fun p
getDecoPoint True (SuggestedSSD SSD {ssdGetPoint = fun}) p = fun p
getDecoPoint _    _ p = p

