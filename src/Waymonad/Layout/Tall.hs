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
module Waymonad.Layout.Tall
where

import Data.Set (Set)

import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..))
import Graphics.Wayland.WlRoots.Render.Color (colorWhite, colorBlack)

import Waymonad.Layout.Ratio
import Waymonad.ViewSet
import Waymonad.Utility.SSD
import Waymonad.Types (SSDPrio (..), ServerSideDecoration (..))
import Waymonad.Types.Core (Seat, View)

import qualified Data.Set as S

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
    pureLayout (Tall ratio) vs ws box = 
        layoutTall ratio box (_asList vs ws)

layoutTall :: Double -> WlrBox -> [(Set Seat, View)] -> [(View, SSDPrio, WlrBox)]
layoutTall _ box [(s, x)] = [(x, NoSSD s, box)]
layoutTall ratio box (x:xs) =
    let unclipped = floor $ fromIntegral (boxWidth box) * ratio
        width = min (boxWidth box - 10) . max 10 $ unclipped
        master = (snd x, sillyDeco 2 $ fst x, box { boxWidth = width  })
        slaves = zip xs [0 ..]
        num = length xs
        height = boxHeight box `div` num
        ibox i = box
            { boxWidth = boxWidth box - width
            , boxX = boxX box + width
            , boxHeight = height
            , boxY = boxY box + i * height
            }
    in master : map (\((s, v), i) -> (v, NoSSD s, ibox i)) slaves
layoutTall _ _ _ = []
