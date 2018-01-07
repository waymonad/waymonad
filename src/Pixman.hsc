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
-- This should probably live in another module in future
module Pixman
    ( pixmanRegionExtents
    , PixmanRegion32
    , PixmanBox32
    )
where

#include <pixman-1/pixman.h>

import Data.Word (Word32)
import Foreing.Ptr (Ptr)
import Foreign.Storable (Storable(..))

data PixmanRegion32

data PixmanBox32 = PixmanBox32
    { pBoxX1 :: Word32
    , pBoxY1 :: Word32
    , pBoxX2:: Word32
    , pBoxY2 :: Word32
    } deriving (Show, Eq)

instance Storable PixmanBox32 where
    sizeOf _ = #{size struct pixman_region32}
    alignment _ = #{alignment struct pixman_region32}
    peek ptr = PixmanBox32
        <$> #{peek struct pixman_region32, x1} ptr
        <*> #{peek struct pixman_region32, y1} ptr
        <*> #{peek struct pixman_region32, x2} ptr
        <*> #{peek struct pixman_region32, y2} ptr
    poke ptr (PixmanBox32 x1 y1 x2 y2} do
        #{poke struct pixman_region32, x1} ptr x1
        #{poke struct pixman_region32, y1} ptr y1
        #{poke struct pixman_region32, x2} ptr x2
        #{poke struct pixman_region32, y2} ptr y2

foreign import ccall unsafe "pixman_region32_extents" c_32_extends :: Ptr PixmanRegion32 -> IO (Ptr PixmanBox32)

pixmanRegionExtents :: Ptr PixmanRegion32 -> IO PixmanBox32
pixmanRegionExtents = fmap peek . c_32_extends
