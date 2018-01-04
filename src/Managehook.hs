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

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), ask, lift)
import Data.Default (Default(..))
import Data.Semigroup (Semigroup (..))

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Input.Seat
import Utility (whenJust)
import View
import ViewSet (WSTag, addView)
import Waymonad
import WayUtil
import WayUtil.Floating
import WayUtil.ViewSet (modifyCurrentWS, modifyWS)
import WayUtil.Current (getCurrentWS)

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

instance Semigroup (InsertAction a) where
    InsertNone <> x = x
    i <> _ = i

instance Default (InsertAction a) where
    def = InsertNone

instance Monoid (InsertAction a) where
    mempty = def
    l `mappend` r = l <> r

type Managehook a = Query a (InsertAction a)

enactInsert
    :: WSTag a
    => InsertAction a
    -> Query a ()
enactInsert act = do
    view <- ask
    liftWay $ case act of
        InsertNone -> pure ()
        InsertFocused -> do
            modifyCurrentWS (flip addView view . Just)
            sendEvent . WSEnter view =<< getCurrentWS
        InsertInto ws -> do
            seat <- getSeat
            modifyWS (addView seat view) ws
            sendEvent $ WSEnter view ws
        InsertFloating box -> do
            setFloating view box
            seat <- getSeat
            liftIO $ whenJust seat $ void . flip keyboardEnter view
        InsertCustom ins -> ins


query :: Query a View
query = ask
