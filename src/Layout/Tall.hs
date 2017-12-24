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
{-# LANGUAGE OverloadedStrings #-}
module Layout.Tall
where

import Data.Foldable (toList)

import Graphics.Wayland.WlRoots.Box (WlrBox(..))

import ViewSet

data Tall = Tall

instance LayoutClass Tall where
    description _ = "Tall"
    handleMessage _ _ = Nothing
    broadcastMessage _ _ = Nothing
    pureLayout _ box zipper = case toList zipper of
        [x] -> [(x, box)]
        (x:xs)->
            let width = boxWidth box `div` 2
                master = (x, box { boxWidth = width  })
                slaves = zip xs [0 ..]
                num = length xs
                height = boxHeight box `div` num
                ibox i = box
                    { boxWidth = width
                    , boxX = boxX box + width
                    , boxHeight = height
                    , boxY = boxY box + i * height
                    }
             in master : map (\(v, i) -> (v, ibox i)) slaves
        [] -> []
