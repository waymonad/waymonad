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
module Layout.Choose
    ( Choose
    , NextLayout (FirstLayout, NextLayout)
    , (|||)
    )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import ViewSet

import qualified Data.Text as T

(|||) :: l -> r -> Choose l r
(|||) = Choose L
infixr 5 |||


data LR = L | R deriving (Eq, Show)

data Choose l r = Choose LR l r deriving (Eq, Show)

data NextLayout = FirstLayout | NextLayout | NoWrap deriving (Eq, Show)

instance Message NextLayout

handle :: LayoutClass l => NextLayout -> l -> Maybe l
handle m l = handleMessage l (SomeMessage m)

choose :: Choose l r -> Maybe l -> Maybe r -> Maybe (Choose l r)
choose _ Nothing Nothing = Nothing
choose (Choose lr l r) nl nr = Just $ Choose lr (fromMaybe l nl) (fromMaybe r nr)

handleNext :: (LayoutClass l, LayoutClass r) => NextLayout -> Choose l r -> Maybe (Choose l r)
handleNext FirstLayout (Choose _ l r) = choose (Choose L l r) (handle FirstLayout l) (Just r)
handleNext NoWrap (Choose L l r) = case handle NoWrap l of
    Just nl -> Just (Choose L nl r)
    Nothing -> choose (Choose R l r) (Just l) (handle FirstLayout r)
handleNext NoWrap (Choose R l r) = Choose R l <$> handle NoWrap r
handleNext NextLayout c = handle NoWrap c <|> handle FirstLayout c

instance (LayoutClass l, LayoutClass r) =>  LayoutClass (Choose l r) where
    handleMessage :: Choose l r -> SomeMessage -> Maybe (Choose l r)
    handleMessage c m
        | Just msg <- getMessage m = handleNext msg c
        | otherwise = case c of
                        Choose L l _ -> choose c (handleMessage l m) Nothing
                        Choose R _ r -> choose c Nothing (handleMessage r m)
    broadcastMessage :: Choose l r -> SomeMessage -> Maybe (Choose l r)
    broadcastMessage = handleMessage
    description :: Choose l r -> Text
    description (Choose _ l r) = description l `T.append` " ||| " `T.append` description r
    currentDesc :: Choose l r -> Text
    currentDesc (Choose L l _) = currentDesc l
    currentDesc (Choose R _ r) = currentDesc r


instance (GenericLayoutClass l vs ws, GenericLayoutClass r vs ws) => GenericLayoutClass (Choose l r) vs ws where
    pureLayout (Choose L l _) vs ws b = pureLayout l vs ws b
    pureLayout (Choose R _ r) vs ws b = pureLayout r vs ws b
