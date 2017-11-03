{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module ViewSet
where

import Data.Foldable (toList)
import Data.List (find)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Typeable
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Box (boxContainsPoint, Point, WlrBox)
import Graphics.Wayland.WlRoots.Seat (WlrSeat)
import View (View)

type ViewSet a = Map a Workspace

newtype Zipper a b = Zipper [(Maybe a, b)]
    deriving (Eq, Show, Functor, Foldable, Traversable)


data Workspace = Workspace
    { wsLayout :: Layout
    , wsViews :: Maybe (Zipper (Ptr WlrSeat) View)
    }

getFocused :: Ptr WlrSeat -> Workspace -> Maybe View
getFocused seat (Workspace _ (Just (Zipper z))) =
    fmap snd $ find ((==) (Just seat) . fst) z
getFocused _ _ = Nothing

addView :: Maybe (Ptr WlrSeat) -> View -> Workspace -> Workspace
addView seat v (Workspace l z) = Workspace l $ addElem seat v z

rmView :: View -> Workspace -> Workspace
rmView v (Workspace l z) = Workspace l $ rmElem v z

viewBelow :: Traversable t => Point -> t (View, WlrBox) -> IO (Maybe View)
viewBelow point views = do
    let candidates = filter (boxContainsPoint point . snd) $ toList views
    pure . listToMaybe . map fst $ candidates

class (Show a, Eq a, Ord a) => WSTag a where
    getName :: a -> Text

instance WSTag Text where
    getName = id

class LayoutClass a where
    pureLayout :: a -> WlrBox -> Zipper b c -> [(c, WlrBox)]
    handleMessage :: a -> SomeMessage -> Maybe a
    description :: a -> Text

data Layout = forall l. LayoutClass l => Layout l

class Typeable m => Message m

data SomeMessage = forall m. Message m => SomeMessage m

addElem' :: Eq a => Maybe a -> Maybe (Zipper a b) -> b -> Zipper a b
addElem' _ Nothing v = Zipper [(Nothing, v)]
addElem' Nothing (Just (Zipper xs)) v = Zipper $ (Nothing, v) : xs
addElem' (Just t) (Just (Zipper xs)) v = 
    let pre = takeWhile ((/=) (Just t) . fst) xs
        pos = dropWhile ((/=) (Just t) . fst) xs
     in Zipper $ case pos of
            [] -> (Just t, v) : pre
            ((_, c):ys) -> pre ++ (Just t, v) : (Nothing, c) : ys

-- This asumes the element is in the zipper only once!
rmElem' :: Eq a => a -> Zipper b a -> Maybe (Zipper b a)
rmElem' y z@(Zipper [(_, x)]) = if x == y
    then Nothing
    else Just z
rmElem' y z@(Zipper xs) =
    let pre = takeWhile ((/=) y . snd) xs
        pos = dropWhile ((/=) y . snd) xs
     in Just $ case pos of
            [] -> z
            ((Nothing, _):ys) -> Zipper $ pre ++ ys
            [(Just t, _)] -> Zipper $ (Just t, snd $ head pre) : tail pre
            ((Just t, _):ys) -> Zipper $ pre ++ (Just t, snd $ head ys) : tail ys

addElem :: Eq a => Maybe a -> b -> Maybe (Zipper a b) -> Maybe (Zipper a b)
addElem t v z = Just $ addElem' t z v

rmElem :: Eq a => a -> Maybe (Zipper b a) -> Maybe (Zipper b a)
rmElem v z = rmElem' v =<< z

contains :: Eq a => a -> Zipper b a -> Bool
contains x (Zipper xs) = elem x $ map snd xs

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

moveRight :: (Ptr WlrSeat) -> Workspace -> Workspace
moveRight t (Workspace l z) = Workspace l $ fmap (moveRight' t) z

moveRight' :: Eq a => a -> Zipper a b -> Zipper a b
moveRight' _ z@(Zipper [_]) = z
moveRight' t (Zipper xs) =
    let pre = takeWhile ((/=) (Just t) . fst) xs
        pos = dropWhile ((/=) (Just t) . fst) xs
     in Zipper $ case pos of
            [] -> (Just t, snd $ head xs) : tail xs
            [(Just _, c)] -> (Just t, snd $ head pre) : tail pre ++ [(Nothing, c)]
            ((Just _, c):ys) -> pre ++ (Nothing ,c): (Just t, snd $ head ys) : tail ys

            -- This case should be impossible
            ((Nothing, _):_) -> error "moveRight hit an impossible case"

moveLeft :: Ptr WlrSeat -> Workspace -> Workspace
moveLeft t (Workspace l z) = Workspace l $ fmap (moveLeft' t) z

moveLeft' :: Eq a => a -> Zipper a b -> Zipper a b
moveLeft' _ z@(Zipper [_]) = z
moveLeft' t (Zipper xs) =
    let pre = takeWhile ((/=) (Just t) . fst) xs
        pos = dropWhile ((/=) (Just t) . fst) xs
     in Zipper $ case pre of

            [] -> init xs ++ [(Just t, snd $ last xs)]
            [(Just _, c)] -> (Just t, c) : (Nothing, snd $ head pos) : tail pos
            ys -> init ys ++ (Nothing, snd $ last ys) : (Nothing, snd $ head pos) : tail pos
