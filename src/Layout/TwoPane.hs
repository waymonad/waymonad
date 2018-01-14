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
module Layout.TwoPane
where

import Data.Foldable (toList)

import Graphics.Wayland.WlRoots.Box (WlrBox(..))

import ViewSet

import qualified Data.Set as S

data TwoPane = TwoPane

instance LayoutClass TwoPane where
    description _ = "TwoPane"
    handleMessage _ _ = Nothing
    broadcastMessage _ _ = Nothing

instance ListLike vs ws => GenericLayoutClass TwoPane vs ws where
    pureLayout _ vs ws box = case _asList vs ws of
        [x] -> [(snd x, box)]
        (x:ys@(y:_))->
            let width = boxWidth box `div` 2
                focused = snd <$> filter (not . S.null . fst) ys
                xFocused = not . S.null . fst $ x
                master = if xFocused || length focused < 2 then snd x else head focused
                secondary = case focused of
                                [] -> snd y
                                (z:[]) -> z
                                (z:z2:_) -> if xFocused then z else z2
             in [(master, box { boxWidth = width }), (secondary, box { boxX = boxX box + width, boxWidth = boxWidth box - width })]
        [] -> []
