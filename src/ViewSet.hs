{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module ViewSet
where

import Data.Typeable
import Data.Text (Text)
import Graphics.Wayland.WlRoots.Box (boxContainsPoint, Point, WlrBox)
import View (View)
import Data.Maybe (listToMaybe)

import Data.Foldable (toList)

import Data.Map (Map)

type ViewSet a = Map a Workspace

data Workspace = Workspace
    { wsLayout :: Layout
    , wsViews :: Maybe (Zipper View)
    }

addView :: View -> Workspace -> Workspace
addView v (Workspace l z) = Workspace l $ addElem v z

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
    pureLayout :: a -> WlrBox -> Zipper b -> [(b, WlrBox)]
    handleMessage :: a -> SomeMessage -> Maybe a
    description :: a -> Text

data Layout = forall l. LayoutClass l => Layout l

class Typeable m => Message m

data SomeMessage = forall m. Message m => SomeMessage m

data Full = Full

instance LayoutClass Full where
    description _ = "Full"
    handleMessage _ _ = Nothing
    pureLayout _ box zipper  = [(zipFoc zipper, box)]

addElem' :: Maybe (Zipper a) -> a -> Zipper a
addElem' Nothing v = Zipper [] v []
addElem' (Just (Zipper pre foc pos)) v = Zipper pre foc (v:pos)

-- This asumes the element is in the zipper only once!
rmElem' :: Eq a => a -> Zipper a -> Maybe (Zipper a)
rmElem' y z@(Zipper [] x []) = if x == y
    then Nothing
    else Just z
rmElem' y (Zipper [] x xs) = if x == y
    then Just $ Zipper [] (head xs) (tail xs)
    else Just $ Zipper [] x (filter (/= y) xs)
rmElem' y (Zipper xs x ys) = if x == y
    then Just $ Zipper (init xs) (last xs) []
    else Just $ Zipper (filter (/= y) xs) x (filter (/= y) ys)


addElem :: a -> Maybe (Zipper a) -> Maybe (Zipper a)
addElem v z = Just $ addElem' z v

rmElem :: Eq a => a -> Maybe (Zipper a) -> Maybe (Zipper a)
rmElem v z = rmElem' v =<< z

data Zipper a = Zipper
    { zipPre :: [a]
    , zipFoc :: !a
    , zipPos :: [a]
    } deriving (Eq, Show)

contains :: Eq a => a -> Zipper a -> Bool
contains x (Zipper pre y pos) =
    x == y
    || x `elem` pre
    || x `elem` pos


instance Functor Zipper where
    fmap f (Zipper pre foc pos) = Zipper (fmap f pre) (f foc) (fmap f pos)

instance Foldable Zipper where
    foldr fun start (Zipper pre foc pos) =
        let right = foldr fun start pos
            middle = foc `fun` right
         in foldr fun middle pre
    foldl fun start (Zipper pre foc pos) =
        let left = foldl fun start pre
            middle = left `fun` foc
         in foldl fun middle pos

instance Traversable Zipper where
    traverse :: Applicative f => (a -> f b) -> Zipper a -> f (Zipper b)
    traverse fun (Zipper pre foc pos) =
        let left = traverse fun pre
            middle = fun foc
            right = traverse fun pos
         in Zipper <$> left <*> middle <*> right
    mapM :: Monad m => (a -> m b) -> Zipper a -> m (Zipper b)
    mapM fun (Zipper pre foc pos) = do
        left <- mapM fun pre
        middle <- fun foc
        right <- mapM fun pos
        pure $ Zipper left middle right

