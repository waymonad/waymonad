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
module Layout.Spiral
where

import Data.Foldable (toList)
import Data.Text (Text)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import ViewSet (LayoutClass (..), Zipper(..), SomeMessage)


data Spiral = Spiral

doLayout :: Int -> WlrBox -> [c] -> [(c, WlrBox)]
doLayout _ _ [] = []
doLayout _ b [x] = [(x, b)]
doLayout 0 b@WlrBox{boxWidth = width, boxX = x} (z:zs) =
    let used = floor $ fromIntegral width * (0.618 :: Double)
     in (z, b {boxWidth = used}) : doLayout 1 b {boxWidth = width - used, boxX = x + used} zs
doLayout 1 b@WlrBox{boxHeight = height, boxY = y} (z:zs) =
    let used = floor $ fromIntegral height * (0.618 :: Double)
     in (z, b {boxHeight = used}) : doLayout 2 b {boxHeight = height - used, boxY = y + used} zs
doLayout 2 b@WlrBox{boxWidth = width, boxX = x} (z:zs) =
    let used = floor $ fromIntegral width * (0.618 :: Double)
     in (z, b {boxWidth = used, boxX = x + width - used}) : doLayout 3 b {boxWidth = width - used} zs
doLayout _ b@WlrBox{boxHeight = height, boxY = y} (z:zs) =
    let used = floor $ fromIntegral height * (0.618 :: Double)
     in (z, b {boxHeight = used, boxY = y + height - used}) : doLayout 0 b {boxHeight = height - used} zs

instance LayoutClass Spiral where
    handleMessage :: Spiral -> SomeMessage -> Maybe Spiral
    handleMessage _ _ = Nothing
    broadcastMessage :: Spiral -> SomeMessage -> Maybe Spiral
    broadcastMessage = handleMessage
    description :: Spiral -> Text
    description _ = "Spiral"
    pureLayout :: Spiral -> WlrBox -> Zipper b c -> [(c, WlrBox)]
    pureLayout _ box z = doLayout 0 box (toList z)
