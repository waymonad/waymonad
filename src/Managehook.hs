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
module Managehook
    ( Query
    , runQuery
    , liftWay
    , enactInsert

    , Managehook
    , InsertAction (..)
    , query
    , withView
    , insertView
    , removeView
    )
where

import Control.Monad (void, forM_)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), ask, lift)

import Input.Seat
import Utility (doJust)
import View
import ViewSet (WSTag)
import Waymonad
import Waymonad.Types
import Layout
import ViewSet
import WayUtil.Floating
import WayUtil.Focus (focusView)
import WayUtil.ViewSet (forceFocused)
import WayUtil.Current (getCurrentWS)

import qualified WayUtil.ViewSet as VS

import qualified Data.Set as S

liftWay :: Way vs a b -> Query vs a b
liftWay = Query . lift

withView :: (View -> Way vs a b) -> Query vs a b
withView act = liftWay . act =<< ask

runQuery :: View -> Query vs a b -> Way vs a b
runQuery v (Query m) = runReaderT m v

enactInsert
    :: (FocusCore vs a, WSTag a)
    => InsertAction vs a
    -> Query vs a ()
enactInsert act = do
    view <- ask
    hook <- wayHooksVWSChange . wayCoreHooks <$> liftWay getState
    liftWay $ case act of
        InsertNone -> pure ()
        InsertFocused -> do
            ws <- getCurrentWS
            seat <- getSeat
            VS.insertView view ws seat
            focusView view
            hook $ WSEnter view ws
        InsertInto ws -> do
            seat <- getSeat
            VS.insertView view ws seat
            hook $ WSEnter view ws
        InsertFloating box -> do
            setFloating view box
            doJust getSeat $ void . flip keyboardEnter view
        InsertCustom ins -> ins


query :: Query vs a View
query = ask

insertView
    :: (FocusCore vs a, WSTag a)
    => View
    -> Way vs a ()
insertView v = do
    hook <- wayManagehook <$> getState
    runQuery v $ enactInsert . flip mappend InsertFocused =<< hook

removeView
    :: (FocusCore vs a, WSTag a)
    => View
    -> Way vs a ()
removeView v = do
    vs <- getViewSet
    forM_ (getVSWorkspaces vs) $ \ws -> do
        VS.removeView v ws
        forceFocused
        reLayout ws

    modifyFloating (S.delete v)
