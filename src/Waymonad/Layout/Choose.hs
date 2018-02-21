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
module Waymonad.Layout.Choose
    ( Choose
    , NextLayout (FirstLayout, NextLayout)
    , (|||)
    )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Waymonad.Types.Core (Seat)
import Waymonad.ViewSet

import qualified Data.Text as T

(|||) :: l -> r -> Choose l r
(|||) = Choose L
infixr 5 |||


data LR = L | R deriving (Eq, Show)

data Choose l r = Choose LR l r deriving (Eq, Show)

data NextLayout = FirstLayout | NextLayout | NoWrap deriving (Eq, Show)

instance Message NextLayout


handle :: LayoutClass l => NextLayout -> Maybe Seat -> l -> Maybe l
handle m s l = handleMessage l s (SomeMessage m)

choose :: Choose l r -> Maybe l -> Maybe r -> Maybe (Choose l r)
choose _ Nothing Nothing = Nothing
choose (Choose lr l r) nl nr = Just $ Choose lr (fromMaybe l nl) (fromMaybe r nr)

handleNext :: (LayoutClass l, LayoutClass r) => NextLayout -> Maybe Seat -> Choose l r -> Maybe (Choose l r)
handleNext FirstLayout s  (Choose _ l r) = choose (Choose L l r) (handle FirstLayout s l) (Just r)
handleNext NoWrap s (Choose L l r) = case handle NoWrap s l of
    Just nl -> Just (Choose L nl r)
    Nothing -> choose (Choose R l r) (Just l) (handle FirstLayout s r)
handleNext NoWrap s (Choose R l r) = Choose R l <$> handle NoWrap s r
handleNext NextLayout s c = handle NoWrap s c <|> handle FirstLayout s c

instance (LayoutClass l, LayoutClass r) =>  LayoutClass (Choose l r) where
    handleMessage :: Choose l r -> Maybe Seat -> SomeMessage -> Maybe (Choose l r)
    handleMessage c s m
        | Just msg <- getMessage m = handleNext msg s c
        | otherwise = case c of
                        Choose L l _ -> choose c (handleMessage l s m) Nothing
                        Choose R _ r -> choose c Nothing (handleMessage r s m)
    --TODO: Implement this broadcast
    broadcastMessage :: Choose l r -> SomeMessage -> Maybe (Choose l r)
    broadcastMessage l m = handleMessage l Nothing m
    description :: Choose l r -> Text
    description (Choose _ l r) = description l `T.append` " ||| " `T.append` description r
    currentDesc :: Choose l r -> Text
    currentDesc (Choose L l _) = currentDesc l
    currentDesc (Choose R _ r) = currentDesc r


instance (GenericLayoutClass l vs ws, GenericLayoutClass r vs ws) => GenericLayoutClass (Choose l r) vs ws where
    pureLayout (Choose L l _) vs ws b = pureLayout l vs ws b
    pureLayout (Choose R _ r) vs ws b = pureLayout r vs ws b
