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
{-# LANGUAGE LambdaCase #-}
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
    , modifyWS
    , getFocused
    )
where

import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.List (nub)

import Waymonad.Input.Seat (Seat, keyboardEnter, keyboardClear, getKeyboardFocus)
import Layout (reLayout)
import Output (Output)
import Utility (whenJust, doJust, These (..))
import View (View, activateView, setViewFocus, unsetViewFocus)
import ViewSet (WSTag, FocusCore (..))
import Waymonad
    ( Way
    , getViewSet
    , getState
    , getSeat
    , WayBindingState (..)
    , makeCallback2
    )
import WayUtil.Current
import WayUtil.Mapping (getOutputKeyboards, setSeatOutput)

import qualified Data.Set as S

-- TODO: Place this better
runLog :: (WSTag a) => Way vs a ()
runLog = do
    state <- getState
    wayLogFunction state

modifyViewSet :: WSTag ws => (vs -> vs) -> Way vs ws ()
modifyViewSet fun = do
    ref <- wayBindingState <$> getState
    liftIO $ modifyIORef ref fun
    runLog

setFocused :: FocusCore vs a => Seat -> a -> Way vs a ()
setFocused seat ws = ((\vs -> _getFocused vs ws $ Just seat) <$> getViewSet) >>= \case
    Just v -> activateView v True >> void (keyboardEnter seat v)
    Nothing -> keyboardClear seat

forceFocused :: (WSTag a, FocusCore vs a) => Way vs a ()
forceFocused = doJust getSeat $ \seat -> do
    ws <- getCurrentWS
    setFocused seat ws

unsetFocus :: FocusCore vs a => Seat -> a -> Way vs a ()
unsetFocus seat ws = doJust ((\vs -> _getFocused vs ws $ Just seat) <$> getViewSet) $
    \v -> activateView v False

getFocused :: FocusCore vs ws => Seat -> ws -> Way vs ws (Maybe View)
getFocused seat ws = withViewSet (\_ vs ->  _getFocused vs ws $ Just seat)

getWSOutputs :: WSTag a => a -> Way vs a [Output]
getWSOutputs ws = do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    pure $ map snd $ filter ((==) ws . fst) mapping

-- | This is a utility function that makes sure things are relayouted/focus is
-- set appropriatly when the modified workspace is displayed
modifyWS
    :: (WSTag a, FocusCore vs a)
    => a
    -> (a -> vs -> vs)
    -> Way vs a ()
modifyWS ws fun = do
    outs <- getWSOutputs ws
    seats <- nub . concat <$> mapM getOutputKeyboards outs

    mapM_ (\case
        Just view -> activateView view False
        Nothing -> pure ()
          ) =<< mapM getKeyboardFocus seats
    modifyViewSet (fun ws)

    mapM_ (`setFocused` ws) seats
    unless (null outs) (reLayout ws >> runLog)

modifyCurrentWS
    :: (WSTag a, FocusCore vs a)
    => (Maybe Seat -> a -> vs -> vs)
    -> Way vs a ()
modifyCurrentWS fun = do
    seatM <- getSeat
    ws <- getCurrentWS
    modifyWS ws (fun seatM)

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

setViewsetFocus :: (WSTag ws, FocusCore vs ws)
                => Seat -> View -> Way vs ws ()
setViewsetFocus seat view = doJust (getPointerOutputS seat) $ \output -> do
    setSeatOutput seat (That output)
    modifyCurrentWS $ \_ ws vs -> _focusView ws seat view vs

insertView :: (FocusCore vs a, WSTag a) => View -> a -> Maybe Seat -> Way vs a ()
insertView v ws s = do
    whenJust s (`unsetFocus` ws)
    setViewFocus v =<< makeCallback2 setViewsetFocus
    modifyWS ws (\ws' -> _insertView ws' s v)

removeView :: (FocusCore vs a, WSTag a) => View -> a -> Way vs a ()
removeView v ws = do
    activateView v False
    unsetViewFocus v
    modifyWS ws (\ws' -> _removeView ws' v)
