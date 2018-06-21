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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : XMonad.ViewSet
Description : An implemention for the ViewSet that aims to recreate the XMonad ViewSet.
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module Waymonad.ViewSet.XMonad
    ( ViewSet
    , Workspace (..)
    , sameLayout
    )
where

import GHC.Prim

import Control.Monad (join)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)

import Waymonad.Utility.Base (whenJust)
import Waymonad.ViewSet

import Waymonad.ViewSet.Utility

import qualified Data.Set as S
import qualified Data.Map as M

type XMWorkspace ws = Workspace (ViewSet ws) ws

-- | The ViewSet type that resembles the XMonad ViewSet. Doesn't use zippers to
-- allow multiseat.
data ViewSet ws = ViewSet
    { _viewSetActive :: Map ws (XMWorkspace ws)
    , _viewSetLayout :: GenericLayout (ViewSet ws) ws
    } deriving (Show)

adjustWS :: Ord ws
         => (XMWorkspace ws -> XMWorkspace ws) -> ws -> ViewSet ws -> ViewSet ws
adjustWS fun ws (ViewSet m l) = ViewSet (M.adjust fun ws m) l

alterWS :: Ord ws
        => (XMWorkspace ws -> XMWorkspace ws) -> ws -> ViewSet ws -> ViewSet ws
alterWS fun ws (ViewSet m l) =
    let empty = Workspace l Nothing
     in ViewSet (M.alter (Just . fun . fromMaybe empty) ws m) l

mapVS :: Ord ws => (XMWorkspace ws -> XMWorkspace ws) -> ViewSet ws -> ViewSet ws
mapVS fun (ViewSet m l) = ViewSet (fun `fmap` m) l

instance Ord ws => Layouted (ViewSet ws) ws where
    {-# SPECIALISE instance Layouted (ViewSet Text) Text #-}
    getLayout (ViewSet vs _) ws = case wsLayout <$> M.lookup ws vs of
        Nothing -> Nothing
        Just (GenericLayout l) -> Just (Layout l)
    broadcastWS m = alterWS modify
        where modify w@(Workspace (GenericLayout l) z) = case broadcastMessage l  m of
                Nothing -> w
                Just nl -> Workspace (GenericLayout nl) z
    messageWS msg s ws vs@(ViewSet m l@(GenericLayout ol)) = case M.lookup ws m of
        Nothing -> case handleMessage ol s msg of

            Nothing -> vs
            Just nl -> ViewSet (M.insert ws (Workspace (GenericLayout nl) Nothing) m) l
        Just (Workspace (GenericLayout wl) z) -> case handleMessage wl s msg of
            Nothing -> vs
            Just nl -> ViewSet (M.insert ws (Workspace (GenericLayout nl) z) m) l
    broadcastVS m _ = mapVS modify
        where modify w@(Workspace (GenericLayout l) z) = case broadcastMessage l  m of
                Nothing -> w
                Just nl -> Workspace (GenericLayout nl) z

instance WSTag a => FocusCore (ViewSet a) a where
    {-# SPECIALISE instance FocusCore (ViewSet Text) Text #-}
    _getFocused (ViewSet vs _) ws (Just s) = getFocused s =<< M.lookup ws vs
    _getFocused (ViewSet vs _) ws Nothing = getFirstFocused =<< M.lookup ws vs
    _focusView ws s v = adjustWS (setFocused v s) ws
    _getViews (ViewSet vs _) ws = fromMaybe mempty $ do
        Workspace _ z <- M.lookup ws vs
        Zipper xs <- z
        pure $ S.fromList xs
    getLayouted vs@(ViewSet m _) ws = whenJust (wsLayout <$> M.lookup ws m) $
        \(GenericLayout l) -> pureLayout l vs ws
    _insertView ws s v vs = alterWS (addView s v) ws vs
    _removeView ws v vs = adjustWS (rmView v) ws vs
    removeGlobal v _ = mapVS (rmView v)
    sameVS _ (ViewSet m l) (ViewSet m' l') =
        case (# reallyUnsafePtrEquality# m m', reallyUnsafePtrEquality# l l' #) of
                (# 1#, 1# #) -> True
                _ -> False

instance WSTag a => ListLike (ViewSet a) a where
    {-# SPECIALISE instance ListLike (ViewSet Text) Text #-}
    _asList (ViewSet vs _) ws =
        join . maybeToList $ fmap unZipper (wsViews =<< M.lookup ws vs)
    _moveFocusLeft ws s vs    = adjustWS (moveLeft s) ws vs
    _moveFocusRight ws s vs   = adjustWS (moveRight s) ws vs
    _moveFocusedLeft ws s vs  = adjustWS (moveViewLeft s) ws vs
    _moveFocusedRight ws s vs = adjustWS (moveViewRight s) ws vs

-- | Create a 'ViewSet' that uses the same layout on all workspace
sameLayout :: (WSTag ws, GenericLayoutClass l (ViewSet ws) ws)
           => l -> ViewSet ws
sameLayout l = ViewSet mempty (GenericLayout l)

