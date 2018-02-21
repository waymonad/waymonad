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
module Waymonad.Layout.TwoPane
where

import Waymonad.Layout.Ratio
import Waymonad.Layout.Tall
import Waymonad.ViewSet

import qualified Data.Set as S

data TwoPane = TwoPane Double

instance LayoutClass TwoPane where
    description _ = "TwoPane"
    handleMessage (TwoPane val) _ m = case getMessage m of
        Just (IncreaseRatio x) -> Just . TwoPane $ val + x
        Just (DecreaseRatio x) -> Just . TwoPane $ val - x
        _ -> Nothing
    broadcastMessage (TwoPane val) m = case getMessage m of
        Just (IncreaseRatio x) -> Just . TwoPane $ val + x
        Just (DecreaseRatio x) -> Just . TwoPane $ val - x
        _ -> Nothing

instance ListLike vs ws => GenericLayoutClass TwoPane vs ws where
    pureLayout (TwoPane ratio) vs ws box = case _asList vs ws of
        (x:ys@(y:_))->
            let focused = filter (not . S.null . fst) ys
                xFocused = not . S.null . fst $ x
                master = if xFocused || length focused < 2 then x else head focused
                secondary = case focused of
                                [] -> y
                                (z:[]) -> z
                                (z:z2:_) -> if xFocused then z else z2

             in layoutTall ratio box [master, secondary]
        zs -> layoutTall ratio box zs
