{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Managehook
    ( Query
    , runQuery
    , liftWay
    , enactInsert

    , Managehook
    , InsertAction (..)
    , query
    )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), ask, lift)

import View
import ViewSet (WSTag, addView)
import Waymonad
import WayUtil

newtype Query a b = Query (ReaderT View (Way a) b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader View)

instance Monoid b => Monoid (Query a b) where
    mempty = pure mempty
    left `mappend` right = mappend <$> left <*> right

liftWay :: Way a b -> Query a b
liftWay = Query . lift

runQuery :: View -> Query a b -> Way a b
runQuery v (Query m) = runReaderT m v

data InsertAction a
    = InsertNone
    | InsertFocused
    | InsertInto a
    | InsertCustom (Way a ())
    deriving (Show)

instance Monoid (InsertAction a) where
    mempty = InsertNone
    InsertNone `mappend` x = x
    i `mappend` _ = i

type Managehook a = Query a (InsertAction a)

enactInsert
    :: WSTag a
    => InsertAction a
    -> Query a ()
enactInsert act = do
    view <- ask
    liftWay $ case act of
        InsertNone -> pure ()
        InsertFocused -> modifyCurrentWS (flip addView view . Just)
        InsertInto ws -> modifyWS (flip addView view . Just) ws
        InsertCustom act -> act


query :: Query a View
query = ask
