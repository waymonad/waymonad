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
module WayUtil.ViewSet
    ( modifyViewSet
    , modifyWS
    , modifyCurrentWS
    , forceFocused
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (fromJust)
import Data.IORef (modifyIORef)

import Input.Seat (Seat, keyboardEnter)
import Layout (reLayout)
import Utility (whenJust, doJust)
import View (View, activateView)
import ViewSet (ViewSet, Workspace (..), Zipper (..), WSTag, getFocused)
import Waymonad
    ( Way
    , getViewSet
    , WayLoggers (..)
    , getState
    , getSeat
    , WayBindingState (..)
    )
import WayUtil.Log (logPutStr)
import WayUtil.Current

import qualified Data.Map as M

-- TODO: Place this better
runLog :: (WSTag a) => Way a ()
runLog = do
    state <- getState
    wayLogFunction state

setFocus :: MonadIO m => (Maybe (Seat), View) -> m ()
setFocus (Nothing, v) = liftIO $ activateView v False
setFocus (Just s, v) = liftIO $ do
    activateView v True
    keyboardEnter s v

setFoci :: MonadIO m => Workspace -> m ()
setFoci (Workspace _ Nothing) = pure ()
setFoci (Workspace _ (Just (Zipper xs))) = mapM_ setFocus xs

modifyViewSet :: (ViewSet a -> ViewSet a) -> Way a ()
modifyViewSet fun = do
    ref <- wayBindingState <$> getState
    liftIO $ modifyIORef ref fun

modifyWS
    :: (WSTag a)
    => (Seat -> Workspace -> Workspace)
    -> a
    -> Way a ()
modifyWS fun ws = do
    logPutStr loggerWS $ "Changing contents of workspace: " ++ show ws
    (Just seat) <- getSeat

    preWs <- getFocused seat . fromJust . M.lookup ws <$> getViewSet
    modifyViewSet (M.adjust (fun seat) ws)
    reLayout ws
    postWs <- getFocused seat . fromJust . M.lookup ws <$> getViewSet

    liftIO $ when (preWs /= postWs) $ whenJust postWs $ \v ->
        keyboardEnter seat v

forceFocused :: WSTag a => Way a ()
forceFocused = doJust (withCurrentWS $ const setFoci) id


modifyCurrentWS
    :: (WSTag a)
    => (Seat -> Workspace -> Workspace) -> Way a ()
modifyCurrentWS fun = do
    modifyWS fun =<< getCurrentWS
    forceFocused
    runLog
