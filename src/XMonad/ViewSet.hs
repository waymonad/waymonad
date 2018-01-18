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
module XMonad.ViewSet
    ( ViewSet
    , Workspace (..)
    , sameLayout
    )
where

import Control.Monad (join)
import Data.List (find)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Set (Set)

import Input.Seat (Seat)
import Utility (whenJust)
import View (View)
import ViewSet

import qualified Data.Set as S
import qualified Data.Map as M

newtype Zipper a b = Zipper { unZipper :: [(Set a, b)] }
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Internal Workspace representation for the 'ViewSet'
data Workspace a = Workspace
    { wsLayout :: GenericLayout (ViewSet a) a
    , wsViews :: Maybe (Zipper Seat View)
    } deriving (Show)

-- | The ViewSet type that resembles the XMonad ViewSet. Doesn't use zippers to
-- allow multiseat.
type ViewSet a = Map a (Workspace a)

instance Ord a => Layouted (ViewSet a) a where
    getLayout vs ws = case wsLayout <$> M.lookup ws vs of
        Nothing -> Nothing
        Just (GenericLayout l) -> Just (Layout l)
    broadcastWS m = M.adjust modify
        where modify w@(Workspace (GenericLayout l) z) = case broadcastMessage l  m of
                Nothing -> w
                Just nl -> Workspace (GenericLayout nl) z
    messageWS m = M.adjust modify
        where modify w@(Workspace (GenericLayout l) z) = case handleMessage l  m of
                Nothing -> w
                Just nl -> Workspace (GenericLayout nl) z
    broadcastVS m _ = fmap modify
        where modify w@(Workspace (GenericLayout l) z) = case broadcastMessage l  m of
                Nothing -> w
                Just nl -> Workspace (GenericLayout nl) z

instance WSTag a => FocusCore (ViewSet a) a where
    _getFocused vs ws (Just s) = getFocused s =<< M.lookup ws vs
    _getFocused vs ws Nothing = getFirstFocused =<< M.lookup ws vs
    _focusView ws s v = M.adjust (setFocused v s) ws
    _getViews vs ws = fromMaybe mempty $ do
        Workspace _ z <- M.lookup ws vs
        Zipper xs <- z
        pure $ S.fromList xs
    getLayouted vs ws = whenJust (wsLayout <$> M.lookup ws vs) $
        \(GenericLayout l) -> pureLayout l vs ws
    _insertView ws s v vs = M.adjust (addView s v) ws vs
    _removeView ws v vs = M.adjust (rmView v) ws vs
    getVSWorkspaces = fmap fst . M.toList

instance WSTag a => ListLike (ViewSet a) a where
    _asList vs ws = join . maybeToList $ fmap unZipper (wsViews =<< M.lookup ws vs)
    _moveFocusLeft ws s vs = M.adjust (moveLeft s) ws vs
    _moveFocusRight ws s vs = M.adjust (moveRight s) ws vs
    _moveFocusedLeft ws s vs  = M.adjust (moveViewLeft s) ws vs
    _moveFocusedRight ws s vs = M.adjust (moveViewRight s) ws vs

-- | Create a 'ViewSet' that uses the same layout on all workspace
sameLayout :: (WSTag a, GenericLayoutClass l (ViewSet a) a)
           => l -> [a] -> M.Map a (Workspace a)
sameLayout l = M.fromList . map (, Workspace (GenericLayout (l)) Nothing)

setFocused :: View -> Seat -> Workspace a -> Workspace a
setFocused v t (Workspace l z) =
    Workspace l $ fmap (setFocused' t v) z

getFocused :: Seat -> Workspace a -> Maybe View
getFocused seat (Workspace _ (Just (Zipper z))) = snd <$> find (elem seat . fst) z
getFocused _ _ = Nothing

getFirstFocused :: Workspace a -> Maybe View
getFirstFocused (Workspace _ z) = getFirstFocused' =<< z

getFirstFocused' :: Zipper a b -> Maybe b
getFirstFocused' (Zipper z) =
    snd <$> find (not . null . fst) z

addView :: Maybe Seat -> View -> Workspace a -> Workspace a
addView seat v (Workspace l z) = Workspace l $ addElem seat v z

rmView :: View -> Workspace a -> Workspace a
rmView v (Workspace l z) = Workspace l $ rmElem v z


-- TODO: Refactor :(
setFocused' :: (Ord a, Eq b) => a -> b -> Zipper a b -> Zipper a b
setFocused' t v (Zipper xs) =
    Zipper $ map update xs
    where   update (ot, x) = if x == v
                then (t `S.insert` ot, x)
                else (t `S.delete` ot, x)

addElem' :: Ord a => Maybe a -> Maybe (Zipper a b) -> b -> Zipper a b
addElem' t Nothing v = Zipper [(maybe mempty S.singleton t, v)]
addElem' Nothing (Just (Zipper xs)) v = Zipper $ (mempty, v) : xs
addElem' (Just t) (Just (Zipper xs)) v = 
    let pre = takeWhile (not . S.member t . fst) xs
        pos = dropWhile (not . S.member t . fst) xs
     in Zipper $ case pos of
            [] -> (S.singleton t, v) : pre
            ((_, c):ys) -> pre ++ (S.singleton t, v) : (mempty, c) : ys

-- This asumes the element is in the zipper only once!
rmElem' :: (Ord a, Eq b) => b -> Zipper a b -> Maybe (Zipper a b)
rmElem' y z@(Zipper [(_, x)]) = if x == y
    then Nothing
    else Just z
rmElem' y zipper@(Zipper xs) =
    let left = takeWhile ((/=) y . snd) xs
        right = dropWhile ((/=) y . snd) xs
     in Just $ doRemove left right
    where doRemove _ [] = zipper
          doRemove left ((t,_):zs)
                | S.null t = Zipper $ left ++ zs
                | ((ot, n):ns) <- zs = Zipper $ left ++ (t <> ot, n):ns
                | otherwise = Zipper $ (t <> fst (head left), snd $ head left) : tail left

addElem :: Ord a => Maybe a -> b -> Maybe (Zipper a b) -> Maybe (Zipper a b)
addElem t v z = Just $ addElem' t z v

rmElem :: (Ord a, Eq b) => b -> Maybe (Zipper a b) -> Maybe (Zipper a b)
rmElem v z = rmElem' v =<< z

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

moveRight :: Seat -> Workspace a -> Workspace a
moveRight t (Workspace l z) = Workspace l $ fmap (moveRight' t) z

moveRight' :: Ord a => a -> Zipper a b -> Zipper a b
moveRight' _ z@(Zipper [_]) = z
moveRight' t (Zipper xs) =
    let pre = takeWhile (not . S.member t . fst) xs
        pos = dropWhile (not . S.member t . fst) xs
     in Zipper $ case pos of
            [] -> (S.singleton t, snd $ head xs) : tail xs
            [(zs, c)] -> (t `S.insert` zs, snd $ head pre) : tail pre ++ [(t `S.delete` zs, c)]
            ((zs, c):ys) -> pre ++ (t `S.delete` zs ,c): (t `S.insert` fst (head ys), snd $ head ys) : tail ys

moveLeft :: Seat -> Workspace a -> Workspace a
moveLeft t (Workspace l z) = Workspace l $ fmap (moveLeft' t) z

moveLeft' :: Ord a => a -> Zipper a b -> Zipper a b
moveLeft' _ z@(Zipper [_]) = z
moveLeft' t (Zipper xs) =
    let pre = takeWhile (not . S.member t . fst) xs
        pos = dropWhile (not . S.member t . fst) xs
     in Zipper $ case pre of

            [] -> (t `S.delete` fst (head xs), snd $ head xs) : init (tail xs) ++ [(t `S.insert` fst (last xs), snd $ last xs)]
            [(zs, c)] -> (t `S.insert` zs, c) : (t `S.delete` fst (head pos), snd $ head pos) : tail pos
            ys -> init ys ++ (t `S.insert` fst (last ys), snd $ last ys) : case pos of
                ((ts, z):zs) -> (t `S.delete` ts, z) : zs
                [] -> []

moveViewLeft :: Seat -> Workspace a -> Workspace a
moveViewLeft t (Workspace l z) = Workspace l $ fmap (moveElemLeft' t) z

moveElemLeft' :: Eq a => a -> Zipper a b -> Zipper a b
moveElemLeft' _ z@(Zipper [_]) = z
moveElemLeft' t (Zipper xs) =
    let pre = takeWhile (notElem t . fst) xs
        pos = dropWhile (notElem t . fst) xs
     in Zipper $ case pre of
        [] -> snoc (head pos) (tail pos)
        ys -> case pos of
            [] -> ys
            (z:zs) ->  let left = last ys
                        in init ys ++ z : left : zs

moveViewRight :: Seat -> Workspace a -> Workspace a
moveViewRight t (Workspace l z) = Workspace l $ fmap (moveElemRight' t) z

moveElemRight' :: Eq a => a -> Zipper a b -> Zipper a b
moveElemRight' _ z@(Zipper [_]) = z
moveElemRight' t (Zipper xs) =
    let pre = takeWhile (notElem t . fst) xs
        pos = dropWhile (notElem t . fst) xs
     in Zipper $ case pos of
        [] -> xs -- We didn't find a focused view
        (y:ys) -> case ys of
            [] -> y : pre
            (z:zs) ->  pre ++ z : y : zs
