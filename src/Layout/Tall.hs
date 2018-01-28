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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Layout.Tall
where

import Graphics.Wayland.WlRoots.Box (WlrBox(..))

import Layout.Ratio
import ViewSet

data Tall = Tall Double

instance LayoutClass Tall where
    description _ = "Tall"
    handleMessage (Tall val) m = case getMessage m of
        Just (IncreaseRatio x) -> Just . Tall $ val + x
        Just (DecreaseRatio x) -> Just . Tall $ val - x
        _ -> Nothing
    broadcastMessage (Tall val) m = case getMessage m of
        Just (IncreaseRatio x) -> Just . Tall $ val + x
        Just (DecreaseRatio x) -> Just . Tall $ val - x
        _ -> Nothing

instance ListLike vs ws => GenericLayoutClass Tall vs ws where
    pureLayout (Tall ratio) vs ws box = case snd `fmap` _asList vs ws of
        [x] -> [(x, box)]
        (x:xs)->
            let unclipped = floor $ fromIntegral (boxWidth box) * ratio
                width = min (boxWidth box - 10) . max 10 $ unclipped
                master = (x, box { boxWidth = width  })
                slaves = zip xs [0 ..]
                num = length xs
                height = boxHeight box `div` num
                ibox i = box
                    { boxWidth = boxWidth box - width
                    , boxX = boxX box + width
                    , boxHeight = height
                    , boxY = boxY box + i * height
                    }
             in master : map (fmap ibox) slaves
        [] -> []
