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
{-# LANGUAGE OverloadedStrings #-}
module WayUtil.ViewSet
    ( modifyViewSet
    , modifyWS
    , modifyCurrentWS
    , setFocused
    , forceFocused
    , withWS
    , unsetFocus
    )
where

import Control.Monad (when, join)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (modifyIORef)
import Data.Maybe (fromJust)
import Data.Set (Set)

import Input.Seat (Seat, keyboardEnter, keyboardClear)
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
import WayUtil.Log (logPutStr, logPutText, LogPriority(..))
import WayUtil.Current

import qualified Data.Map as M
import qualified Data.Set as S

-- TODO: Place this better
runLog :: (WSTag a) => Way a ()
runLog = do
    state <- getState
    wayLogFunction state

setFocus :: Seat -> (Set Seat, View) -> Way a ()
setFocus s (s', v) = when (s `S.member` s') $ do
    logPutText loggerFocus Trace "Actually setting a focus"
    liftIO $ do
        success <- keyboardEnter s v
        when success $ activateView v True

setFoci :: Seat -> Workspace -> Way a ()
setFoci s (Workspace _ Nothing) = do
    logPutText loggerFocus Trace "Clearing focus for seat"
    keyboardClear s
setFoci s (Workspace _ (Just (Zipper xs))) = do
    logPutText loggerFocus Trace "Setting foci for seat"
    mapM_ (setFocus s) xs

modifyViewSet :: Show a => (ViewSet a -> ViewSet a) -> Way a ()
modifyViewSet fun = do
    ref <- wayBindingState <$> getState
    liftIO $ modifyIORef ref fun
    vs <- getViewSet
    logPutStr loggerWS Debug $ "Changed viewset, now is: " ++ show vs

modifyWS
    :: (WSTag a)
    => (Workspace -> Workspace)
    -> a
    -> Way a ()
modifyWS fun ws = do
    logPutStr loggerWS Debug $ "Changing contents of workspace: " ++ show ws

    modifyViewSet (M.adjust fun ws)
    reLayout ws

setFocused :: WSTag a => Seat -> a -> Way a ()
setFocused seat ws = join . withWS ws $ setFoci seat

forceFocused :: WSTag a => Way a ()
forceFocused = doJust (withCurrentWS $ setFoci) id

unsetFocus' :: MonadIO m => Seat -> (Set Seat, View) -> m ()
unsetFocus' s (s', v) = when (S.singleton s == s') $ do
    activateView v False

unsetFoci :: Seat -> Workspace -> Way a ()
unsetFoci _ (Workspace _ Nothing) = pure ()
unsetFoci s (Workspace _ (Just (Zipper xs))) = mapM_ (unsetFocus' s) xs

unsetFocus :: WSTag a => Seat -> a -> Way a ()
unsetFocus seat ws = join . withWS ws $ unsetFoci seat

modifyCurrentWS
    :: (WSTag a)
    => (Seat -> Workspace -> Workspace) -> Way a ()
modifyCurrentWS fun = doJust getSeat $ \seat -> do
    ws <- getCurrentWS
    preWs <- getFocused seat . fromJust . M.lookup ws <$> getViewSet
    modifyWS (fun seat) ws
    postWs <- getFocused seat . fromJust . M.lookup ws <$> getViewSet
    -- This should really be on the 2 views we know about, not full
    whenJust preWs (flip activateView False)
    whenJust postWs (flip activateView True)
    forceFocused
    runLog

withWS
    :: (WSTag a)
    => a
    -> (Workspace -> b)
    -> Way a b
withWS ws fun = do
    vs <- getViewSet

    pure . fun . fromJust $  M.lookup ws vs
