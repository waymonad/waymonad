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

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), ask, lift)

import Input.Seat
import Utility (whenJust)
import View
import ViewSet (WSTag, addView, rmView, contains, wsViews)
import Waymonad
import Waymonad.Types
import Layout
import WayUtil
import WayUtil.Floating
import WayUtil.ViewSet
    ( modifyCurrentWS, modifyWS, forceFocused, modifyViewSet
    )
import WayUtil.Current (getCurrentWS)

import qualified Data.Map as M
import qualified Data.Set as S

liftWay :: Way a b -> Query a b
liftWay = Query . lift

withView :: (View -> Way a b) -> Query a b
withView act = liftWay . act =<< ask

runQuery :: View -> Query a b -> Way a b
runQuery v (Query m) = runReaderT m v

enactInsert
    :: WSTag a
    => InsertAction a
    -> Query a ()
enactInsert act = do
    view <- ask
    liftWay $ case act of
        InsertNone -> pure ()
        InsertFocused -> do
            modifyCurrentWS (`addView` view)
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

insertView
    :: WSTag a
    => View
    -> Way a ()
insertView v = do
    hook <- wayManagehook <$> getState
    runQuery v $ enactInsert . flip mappend InsertFocused =<< hook

removeView
    :: (WSTag a)
    => View
    -> Way a ()
removeView v = do
    wsL <- filter (maybe False (contains v) . wsViews . snd) . M.toList <$> getViewSet

    case wsL of
        [(ws, _)] -> do
            modifyViewSet $ M.adjust (rmView v) ws
            reLayout ws

            forceFocused
        [] -> pure ()
-- TODO: log an error
        _ -> pure ()
    modifyFloating (S.delete v)
