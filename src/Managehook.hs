{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Managehook
    ( Query
    , runQuery
    , liftWay
    , enactInsert

    , Managehook
    , InsertAction (..)
    , query
    , withView
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), ask, lift)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))
import Graphics.Wayland.WlRoots.Seat (keyboardNotifyEnter)

import Utility (whenJust)
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

withView :: (View -> Way a b) -> Query a b
withView act = liftWay . act =<< ask

runQuery :: View -> Query a b -> Way a b
runQuery v (Query m) = runReaderT m v

data InsertAction a
    = InsertNone
    | InsertFocused
    | InsertInto a
    | InsertFloating WlrBox
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
        InsertFloating box -> do
            setFloating view box
            seat <- getSeat
            surf <- getViewSurface view
            liftIO . whenJust seat $ flip keyboardNotifyEnter surf
        InsertCustom ins -> ins


query :: Query a View
query = ask
