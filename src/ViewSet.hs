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
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module ViewSet
where

import Control.Monad (filterM)
import Data.Foldable (toList)
import Data.List (find, delete)
import Data.Map (Map)
import Data.Maybe (listToMaybe, maybeToList, isJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Graphics.Wayland.WlRoots.Box (boxContainsPoint, Point (..), WlrBox (..))

import Input.Seat (Seat)
import View (View, getViewEventSurface)

import qualified Data.Text as T
import qualified Data.Set as S

type ViewSet a = Map a Workspace

newtype Zipper a b = Zipper [(Set a, b)]
    deriving (Eq, Show, Functor, Foldable, Traversable)


data Workspace = Workspace
    { wsLayout :: Layout
    , wsViews :: Maybe (Zipper Seat View)
    } deriving (Show)

class (Typeable a, Show a, Eq a, Ord a) => WSTag a where
    getName :: a -> Text

instance WSTag Text where
    getName = id

class LayoutClass a where
    pureLayout :: a -> WlrBox -> Zipper b c -> [(c, WlrBox)]
    handleMessage :: a -> SomeMessage -> Maybe a
    broadcastMessage :: a -> SomeMessage -> Maybe a
    description :: a -> Text

data Layout = forall l. LayoutClass l => Layout l

instance Show Layout where
    show (Layout l) = T.unpack $ description l

class Typeable m => Message m

data SomeMessage = forall m. Message m => SomeMessage m

getMessage :: Message m => SomeMessage -> Maybe m
getMessage (SomeMessage m) = cast m

messageWS :: SomeMessage -> Workspace -> Workspace
messageWS m w@(Workspace (Layout l) z) =
    case handleMessage l  m of
        Nothing -> w
        Just nl -> Workspace (Layout nl) z

getMaster :: Workspace -> Maybe View
getMaster (Workspace _ z) = getMaster' =<< z

getMaster' :: Zipper a b -> Maybe b
getMaster' (Zipper xs) =  snd <$> listToMaybe xs

setFocused :: View -> Seat -> Workspace -> Workspace
setFocused v t (Workspace l z) =
    Workspace l $ fmap (setFocused' t v) z

getFocused :: Seat -> Workspace -> Maybe View
getFocused seat (Workspace _ (Just (Zipper z))) =
    fmap snd $ find (elem seat . fst) z
getFocused _ _ = Nothing

getFirstFocused :: Workspace -> Maybe View
getFirstFocused (Workspace _ z) = getFirstFocused' =<< z

getFirstFocused' :: Zipper a b -> Maybe b
getFirstFocused' (Zipper z) =
    fmap snd $ find (not . null . fst) z

addView :: Maybe (Seat) -> View -> Workspace -> Workspace
addView seat v (Workspace l z) = Workspace l $ addElem seat v z

rmView :: View -> Workspace -> Workspace
rmView v (Workspace l z) = Workspace l $ rmElem v z


viewsBelow
    :: Traversable t
    => Point
    -> t (View, WlrBox)
    -> IO [(View, Int, Int)]
viewsBelow (Point x y) views = do
    (map $ uncurry makeLocal) <$> filterM hasSurface (toList views)
    where   makeLocal :: View -> WlrBox -> (View, Int, Int)
            makeLocal view (WlrBox bx by _ _) =
                (view, x - bx, y - by)
            hasSurface :: (View, WlrBox) -> IO Bool
            hasSurface (view, WlrBox bx by _ _) = isJust <$> getViewEventSurface view (fromIntegral (x - bx)) (fromIntegral (y - by))

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
                | otherwise = Zipper $ (t <> (fst $ head left), snd $ head left) : tail left

addElem :: Ord a => Maybe a -> b -> Maybe (Zipper a b) -> Maybe (Zipper a b)
addElem t v z = Just $ addElem' t z v

rmElem :: (Ord a, Eq b) => b -> Maybe (Zipper a b) -> Maybe (Zipper a b)
rmElem v z = rmElem' v =<< z

contains :: Eq a => a -> Zipper b a -> Bool
contains x (Zipper xs) = elem x $ map snd xs

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

moveRight :: (Seat) -> Workspace -> Workspace
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

moveLeft :: Seat -> Workspace -> Workspace
moveLeft t (Workspace l z) = Workspace l $ fmap (moveLeft' t) z

moveLeft' :: Ord a => a -> Zipper a b -> Zipper a b
moveLeft' _ z@(Zipper [_]) = z
moveLeft' t (Zipper xs) =
    let pre = takeWhile (not . S.member t . fst) xs
        pos = dropWhile (not . S.member t . fst) xs
     in Zipper $ case pre of

            [] -> (t `S.delete` (fst $ head xs), snd $ head xs) : init (tail xs) ++ [(t `S.insert` (fst $ last xs), snd $ last xs)]
            [(zs, c)] -> (t `S.insert` zs, c) : (t `S.delete` (fst $ head pos), snd $ head pos) : tail pos
            ys -> init ys ++ (t `S.insert` (fst $ last ys), snd $ last ys) : case pos of
                ((ts, z):zs) -> (t `S.delete` ts, z) : zs
                [] -> []

moveViewLeft :: Seat -> Workspace -> Workspace
moveViewLeft t (Workspace l z) = Workspace l $ fmap (moveElemLeft' t) z

moveElemLeft' :: Eq a => a -> Zipper a b -> Zipper a b
moveElemLeft' _ z@(Zipper [_]) = z
moveElemLeft' t (Zipper xs) =
    let pre = takeWhile (not . elem t . fst) xs
        pos = dropWhile (not . elem t . fst) xs
     in Zipper $ case pre of
        [] -> snoc (head pos) (tail pos)
        ys -> case pos of
            [] -> ys
            (z:zs) ->  let left = last ys
                        in (init ys) ++ z : left : zs

moveViewRight :: Seat -> Workspace -> Workspace
moveViewRight t (Workspace l z) = Workspace l $ fmap (moveElemRight' t) z

moveElemRight' :: Eq a => a -> Zipper a b -> Zipper a b
moveElemRight' _ z@(Zipper [_]) = z
moveElemRight' t (Zipper xs) =
    let pre = takeWhile (not . elem t . fst) xs
        pos = dropWhile (not . elem t . fst) xs
     in Zipper $ case pos of
        [] -> xs -- We didn't find a focused view
        (y:ys) -> case ys of
            [] -> y : pre
            (z:zs) ->  pre ++ z : y : zs
