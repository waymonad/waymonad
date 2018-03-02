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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Waymonad.Layout.Vertical
where

import Data.Set (Set)

import Graphics.Wayland.WlRoots.Box (WlrBox(..))

import Waymonad.ViewSet
import Waymonad.Types (SSDPrio (NoSSD))
import Waymonad.Types.Core (Seat, View)

data Vertical = Vertical

instance LayoutClass Vertical where
    description _ = "Vertical"
    handleMessage _ _ _ = Nothing
    broadcastMessage _ _  = Nothing

instance ListLike vs ws => GenericLayoutClass Vertical vs ws where
    pureLayout _ vs ws box = layoutVertical box (_asList vs ws)

layoutVertical :: WlrBox -> [(Set Seat, View)] -> [(View, SSDPrio, WlrBox)]
layoutVertical box xs =
    let slaves = zip xs [0 ..]
        num = length xs
        height = boxHeight box `div` num
        ibox i = box
            { boxHeight = height
            , boxY = boxY box + i * height
            }
    in map (\((s, v), i) -> (v, NoSSD s, ibox i)) slaves
