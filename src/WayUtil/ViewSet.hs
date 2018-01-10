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
    , setFocused
    , forceFocused
    , unsetFocus
    , getWorkspaces
    , modifyCurrentWS
    , modifyFocusedWS
    , insertView
    , removeView
    , withCurrentWS
    , withViewSet
    , getWorkspaceViews
    )
where

import Control.Monad (when, join, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (modifyIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Set (Set)

import Input.Seat (Seat, keyboardEnter, keyboardClear)
import Layout (reLayout)
import Utility (whenJust, doJust)
import View (View, activateView)
import ViewSet (ViewSet, Workspace (..), Zipper (..), WSTag, getFocused, FocusCore (..))
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
runLog :: (WSTag a) => Way vs a ()
runLog = do
    state <- getState
    wayLogFunction state

setFocus :: Seat -> (Set Seat, View) -> Way vs a ()
setFocus s (s', v) = when (s `S.member` s') $ do
    logPutText loggerFocus Trace "Actually setting a focus"
    liftIO $ do
        success <- keyboardEnter s v
        when success $ activateView v True

modifyViewSet :: (vs -> vs) -> Way vs a ()
modifyViewSet fun = do
    ref <- wayBindingState <$> getState
    liftIO $ modifyIORef ref fun

setFocused :: FocusCore vs a => Seat -> a -> Way vs a ()
setFocused seat ws = doJust ((\vs -> _getFocused vs ws $ Just seat) <$> getViewSet) $
    \v -> activateView v True >> void (keyboardEnter seat v)

forceFocused :: (WSTag a, FocusCore vs a) => Way vs a ()
forceFocused = doJust getSeat $ \seat -> do
    ws <- getCurrentWS
    setFocused seat ws

unsetFocus :: FocusCore vs a => Seat -> a -> Way vs a ()
unsetFocus seat ws = doJust ((\vs -> _getFocused vs ws $ Just seat) <$> getViewSet) $
    \v -> activateView v False

modifyCurrentWS
    :: (WSTag a, FocusCore vs a)
    => (Maybe Seat -> a -> vs -> vs)
    -> Way vs a ()
modifyCurrentWS fun = do
    seatM <- getSeat
    ws <- getCurrentWS
    let getView = whenJust seatM (\seat -> (\vs -> _getFocused vs ws $ Just seat) <$> getViewSet)

    preWs <- getView
    modifyViewSet (fun seatM ws)
    postWs <- getView

    -- This should really be on the 2 views we know about, not full
    whenJust preWs (`activateView` False)
    whenJust postWs (`activateView` True)
    forceFocused
    runLog

modifyFocusedWS
    :: (WSTag ws, FocusCore vs ws)
    => (Seat -> ws -> vs -> vs) -> Way vs ws ()
-- Somwhat ugly hack to make sure getSeat returns a Just value, but I prefer it
-- over code duplication
modifyFocusedWS fun = doJust getSeat $ \_ ->
    modifyCurrentWS (fun . fromJust)

withCurrentWS
    :: (FocusCore vs a)
    => (Maybe Seat -> a -> vs -> b)
    -> Way vs a b
withCurrentWS fun = do
    ws <- getCurrentWS
    withViewSet $ flip fun ws

withViewSet
    :: (FocusCore vs a)
    => (Maybe Seat -> vs -> b)
    -> Way vs a b
withViewSet fun = do
    seat <- getSeat
    vs <- liftIO . readIORef .  wayBindingState =<< getState
    pure $ fun seat vs

getWorkspaces :: Way vs a [a]
getWorkspaces = wayUserWorkspaces <$> getState

getWorkspaceViews :: FocusCore vs a => a -> Way vs a [View]
getWorkspaceViews ws = withViewSet (\_ vs -> fmap snd . S.toList $ _getViews vs ws)

insertView :: (FocusCore vs a, WSTag a) => View -> a -> Maybe Seat -> Way vs a ()
insertView v ws s = modifyViewSet (_insertView ws s v)

removeView :: (FocusCore vs a, WSTag a) => View -> a -> Way vs a ()
removeView v ws = modifyViewSet (_removeView ws v)
