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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Layout.Spiral
where

import Data.Set (Set)
import Data.Text (Text)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Layout.Ratio
import ViewSet
import Waymonad.Types
import Waymonad.Types.Core (Seat)
import WayUtil.SSD

data Spiral = Spiral Double

doLayout :: Spiral -> Int -> WlrBox -> [(Set Seat, c)] -> [(c, SSDPrio, WlrBox)]
doLayout _ _ _ [] = []
doLayout _ _ b [(f, x)] = [(x, sillyDeco 2 f, b)]
doLayout s@(Spiral r) 0 b@WlrBox{boxWidth = width, boxX = x} ((f, z):zs) =
    let used = floor $ fromIntegral width * r
     in (z, sillyDeco 2 f, b {boxWidth = used}) : doLayout s 1 b {boxWidth = width - used, boxX = x + used} zs
doLayout s@(Spiral r) 1 b@WlrBox{boxHeight = height, boxY = y} ((f, z):zs) =
    let used = floor $ fromIntegral height * r
     in (z, sillyDeco 2 f, b {boxHeight = used}) : doLayout s 2 b {boxHeight = height - used, boxY = y + used} zs
doLayout s@(Spiral r) 2 b@WlrBox{boxWidth = width, boxX = x} ((f, z):zs) =
    let used = floor $ fromIntegral width * r
     in (z, sillyDeco 2 f, b {boxWidth = used, boxX = x + width - used}) : doLayout s 3 b {boxWidth = width - used} zs
doLayout s@(Spiral r) _ b@WlrBox{boxHeight = height, boxY = y} ((f, z):zs) =
    let used = floor $ fromIntegral height * r
     in (z, sillyDeco 2 f, b {boxHeight = used, boxY = y + height - used}) : doLayout s 0 b {boxHeight = height - used} zs

instance LayoutClass Spiral where
    handleMessage :: Spiral -> SomeMessage -> Maybe Spiral
    handleMessage (Spiral val) m = case getMessage m of
        Just (IncreaseRatio x) -> Just . Spiral $ min 1 $ val + x
        Just (DecreaseRatio x) -> Just . Spiral $ max 0 $ val - x
        _ -> Nothing
    broadcastMessage :: Spiral -> SomeMessage -> Maybe Spiral
    broadcastMessage = handleMessage
    description :: Spiral -> Text
    description _ = "Spiral"

instance ListLike vs ws => GenericLayoutClass Spiral vs ws where
    pureLayout s vs ws box = doLayout s 0 box $ _asList vs ws
