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
module Layout.ToggleFull
where

import Control.Applicative ((<|>))
import Data.Text (Text)

import Graphics.Wayland.WlRoots.Box (WlrBox)

import ViewSet
    ( Message
    , Zipper
    , LayoutClass (..)
    , SomeMessage
    , getFirstFocused'
    , getMaster'
    , getMessage
    )

import qualified Data.Text as T

data TMessage
    = TMessage
    | SetFull
    | UnsetFull
    deriving (Show, Eq, Message)

data ToggleFull l = ToggleFull Bool l

instance LayoutClass l => LayoutClass (ToggleFull l) where
    handleMessage :: ToggleFull l -> SomeMessage -> Maybe (ToggleFull l)
    handleMessage (ToggleFull state l) m =
        case getMessage m of
            (Just TMessage) -> Just $ ToggleFull (not state) l
            (Just SetFull) -> Just $ ToggleFull True l
            (Just UnsetFull) -> Just $ ToggleFull False l
            Nothing -> ToggleFull state <$> handleMessage l m
    broadcastMessage :: ToggleFull l -> SomeMessage -> Maybe (ToggleFull l)
    broadcastMessage (ToggleFull state l) m = ToggleFull state <$> broadcastMessage l m
    description :: ToggleFull l -> Text
    description (ToggleFull _ l) =
        "ToggleFull(" `T.append` description l `T.append` ")"
    currentDesc :: ToggleFull l -> Text
    currentDesc (ToggleFull False l) = currentDesc l
    currentDesc (ToggleFull True _) = "ToggledFull"
    pureLayout :: ToggleFull l -> WlrBox -> Zipper b c -> [(c, WlrBox)]
    pureLayout (ToggleFull False l) box z = pureLayout l box z
    pureLayout (ToggleFull True _) box z =
        case getFirstFocused' z <|> getMaster' z of
            Nothing -> []
            Just v -> [(v, box)]
