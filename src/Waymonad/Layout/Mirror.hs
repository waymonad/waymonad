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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Waymonad.Layout.Mirror
    ( ToggleMirror (..)
    , Mirror (..)
    , mkMirror
    )
where

import Data.Text (Text)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Waymonad.ViewSet

import qualified Data.Text as T

data ToggleMirror = ToggleMirror
    deriving (Show, Eq, Message)

data Mirror l = Mirror Bool l

mkMirror :: l -> Mirror l
mkMirror = Mirror False

instance LayoutClass l => LayoutClass (Mirror l) where
    handleMessage :: Mirror l -> SomeMessage -> Maybe (Mirror l)
    handleMessage (Mirror state l) m =
        case getMessage m of
            (Just ToggleMirror) -> Just $ Mirror (not state) l
            Nothing -> Mirror state <$> handleMessage l m
    broadcastMessage :: Mirror l -> SomeMessage -> Maybe (Mirror l)
    broadcastMessage (Mirror state l) m = Mirror state <$> broadcastMessage l m
    description :: Mirror l -> Text
    description (Mirror _ l) =
        "Mirror(" `T.append` description l `T.append` ")"
    currentDesc :: Mirror l -> Text
    currentDesc (Mirror True l) =
        "Mirror(" `T.append` currentDesc l `T.append` ")"
    currentDesc (Mirror False l) = currentDesc l


instance GenericLayoutClass l vs ws => GenericLayoutClass (Mirror l) vs ws where
    pureLayout (Mirror False l) vs ws box = pureLayout l vs ws box
    pureLayout (Mirror True l) vs ws box =
        (\(v, d, b) -> (v, d, mirrorBox b)) <$> pureLayout l vs ws (mirrorBox box)

mirrorBox :: WlrBox -> WlrBox
mirrorBox (WlrBox x y w h) = WlrBox y x h w
