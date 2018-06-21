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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Waymonad.ViewSet.Utility
where

import Data.List (find)
import Data.Semigroup ((<>))
import Data.Set (Set)

import Waymonad.ViewSet
import Waymonad.Types.Core (Seat, View)

import qualified Data.Set as S

newtype Zipper a b = Zipper { unZipper :: [(Set a, b)] }
    deriving (Eq, Show)

mergeZippers :: Maybe (Zipper a b) -> Maybe (Zipper a b) -> Maybe (Zipper a b)
mergeZippers Nothing Nothing = Nothing
mergeZippers xs ys = Just $ Zipper
    (maybe [] unZipper xs ++ maybe [] unZipper ys)

-- | Internal Workspace representation for the 'ViewSet'
data Workspace vs ws = Workspace
    { wsLayout :: GenericLayout vs ws
    , wsViews :: Maybe (Zipper Seat View)
    } deriving (Show)

setFocused :: View -> Seat -> Workspace vs ws -> Workspace vs ws
setFocused v t (Workspace l z) =
    Workspace l $ fmap (setFocused' t v) z

getFocused :: Seat -> Workspace vs ws -> Maybe View
getFocused seat (Workspace _ (Just (Zipper z))) = snd <$> find (elem seat . fst) z
getFocused _ _ = Nothing

getFirstFocused :: Workspace ws vs -> Maybe View
getFirstFocused (Workspace _ z) = getFirstFocused' =<< z

getMFocused :: Maybe Seat -> Workspace ws vs -> Maybe View
getMFocused Nothing = getFirstFocused
getMFocused (Just s) = getFocused s

getFirstFocused' :: Zipper a b -> Maybe b
getFirstFocused' (Zipper z) =
    snd <$> find (not . null . fst) z

addView :: Maybe Seat -> View -> Workspace ws vs -> Workspace ws vs
addView seat v (Workspace l z) = Workspace l $ addElem seat v z

rmView :: View -> Workspace ws vs -> Workspace ws vs
rmView v (Workspace l z) = Workspace l $ rmElem v z

zipContains :: (Ord b) => b -> Zipper a b -> Bool
{-# SPECIALISE zipContains :: View -> Zipper a View -> Bool #-}
zipContains b (Zipper xs) = any ((==) b . snd) xs

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

moveRight :: Seat -> Workspace ws vs -> Workspace ws vs
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

moveLeft :: Seat -> Workspace ws vs -> Workspace ws vs
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

moveViewLeft :: Seat -> Workspace ws vs -> Workspace ws vs
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

moveViewRight :: Seat -> Workspace ws vs -> Workspace ws vs
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

instance FocusCore (Zipper Seat View) ws where
    _getFocused (Zipper z) _ (Just s) = snd <$> find (elem s . fst) z
    _getFocused z _ Nothing = getFirstFocused' z
    _getViews (Zipper xs) _ = S.fromList xs
    _focusView  = error "_focusView is not supported in Zipper pseudo instance"
    _insertView = error "_insertView is not supported in Zipper pseudo instance"
    _removeView = error "_removeView is not supported in Zipper pseudo instance"
    removeGlobal = error "_removeGlobal is not supported in Zipper pseudo instance"
    getLayouted = error "getLayouted is not supported on the Zipper pseudo instance"
    sameVS _ _ _ = False

instance ListLike (Zipper Seat View) ws where
    _asList (Zipper xs) _ = xs
    _moveFocusLeft = error "_moveFocusLeft is not supported by zipper pseudo instance"
    _moveFocusRight = error "_moveFocusRight is not supported by zipper pseudo instance"
    _moveFocusedLeft = error "_moveFocusedLeft is not supported by zipper pseudo instance"
    _moveFocusedRight = error "_moveFocusedRight is not supported by zipper pseudo instance"

