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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Waymonad.Layout.SmartBorders
where

import Data.Functor.Identity
import Formatting

import Waymonad.ViewSet
import Waymonad.Types
import Waymonad.Utility.SSD

import qualified Data.Text as T

data SetBorderWidth = SetBorderWidth Int deriving (Eq, Show)

instance Message SetBorderWidth

data SmartBorders c l = SmartBorders
    { smartBorderWidth :: c Int
    , smartBorderChild :: l
    }

mkSmartBorders :: Int -> l -> SmartBorders Identity l
mkSmartBorders = SmartBorders . Identity

instance LayoutClass l => LayoutClass (SmartBorders Identity l) where
    handleMessage (SmartBorders state l) m = case getMessage m of
        (Just (SetBorderWidth w)) -> Just $ SmartBorders (Identity w) l
        Nothing -> SmartBorders state <$> handleMessage l m
    broadcastMessage (SmartBorders state l) m = case getMessage m of
        (Just (SetBorderWidth w)) -> Just $ SmartBorders (Identity w) l
        Nothing -> SmartBorders state <$> broadcastMessage l m
    description (SmartBorders _ l) = "SmartBorders(" `T.append` description l `T.append` ")"
    currentDesc (SmartBorders (Identity v) l) = sformat ("SmartBorders[" % int % "]" % stext) v (currentDesc l)

instance (GenericLayoutClass l vs ws) => GenericLayoutClass (SmartBorders Identity l) vs ws where
    pureLayout (SmartBorders (Identity w) l) vs ws box = case pureLayout l vs ws box of

        [] -> []
        [x] -> [x]
        xs -> flip fmap xs (\case
            (v, NoSSD s, b) -> (v, sillyDeco w s, b)
            x -> x)
