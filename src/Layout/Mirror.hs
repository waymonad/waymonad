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
module Layout.Mirror
    ( MMessage (..)
    , Mirror (..)
    )
where

import Data.Text (Text)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import ViewSet
    ( Message
    , Zipper
    , LayoutClass (..)
    , SomeMessage
    , getMessage
    )

import qualified Data.Text as T

data MMessage = MMessage
    deriving (Show, Eq, Message)

data Mirror l = Mirror Bool l

instance LayoutClass l => LayoutClass (Mirror l) where
    handleMessage :: Mirror l -> SomeMessage -> Maybe (Mirror l)
    handleMessage (Mirror state l) m =
        case getMessage m of
            (Just MMessage) -> Just $ Mirror (not state) l
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
    pureLayout :: Mirror l -> WlrBox -> Zipper b c -> [(c, WlrBox)]
    pureLayout (Mirror False l) box z = pureLayout l box z
    pureLayout (Mirror True l) box z =
        fmap (fmap mirrorBox) $ pureLayout l (mirrorBox box) z

mirrorBox :: WlrBox -> WlrBox
mirrorBox (WlrBox x y w h) = WlrBox y x h w
