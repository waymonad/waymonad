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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Utility.ViewSet
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
    , probeVS
    )
where

import Control.Monad (void, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Maybe (fromJust)

import Graphics.Wayland.WlRoots.Output (getOutputPosition)
import Graphics.Wayland.WlRoots.Box (Point (..))

import Waymonad.Input.Seat (Seat, keyboardEnter, keyboardClear, getKeyboardFocus)
import Waymonad.Layout (reLayout, getWSLayout, freezeLayout, sendLayout, applyLayout)
import Waymonad.Utility.Base (whenJust, doJust, These (..))
import Waymonad.View
    (View, activateView , setViewManager, unsetViewManager)
import Waymonad.ViewSet (WSTag, FocusCore (..))
import Waymonad
    ( Way
    , getViewSet
    , getState
    , getSeat
    , WayBindingState (..)
    , unliftWay
    , makeCallback
    , makeCallback2
    )
import Waymonad.Types (Output (..), EvtCause (..))
import Waymonad.Types.Core (ManagerData (..))
import Waymonad.Utility.Current
import Waymonad.Utility.Mapping (getOutputKeyboards, setSeatOutput, getOutputWS, getOutputs)
import Waymonad.Utility.LayerCache (applyLayerDamage, getLayerPosition')

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

setFocused :: FocusCore vs a => Seat -> EvtCause -> a -> Way vs a ()
setFocused seat intent ws = ((\vs -> _getFocused vs ws $ Just seat) <$> getViewSet) >>= \case
    Just v -> activateView v True >> void (keyboardEnter seat intent v)
    Nothing -> keyboardClear seat

forceFocused :: (WSTag a, FocusCore vs a) => EvtCause -> Way vs a ()
forceFocused intent = doJust getSeat $ \seat -> do
    ws <- getCurrentWS
    setFocused seat intent ws

unsetFocus :: FocusCore vs a => Seat -> a -> Way vs a ()
unsetFocus seat ws = doJust ((\vs -> _getFocused vs ws $ Just seat) <$> getViewSet) $ \v -> do
    activateView v False
    keyboardClear seat

getFocused :: FocusCore vs ws => Seat -> ws -> Way vs ws (Maybe View)
getFocused seat ws = withViewSet (\_ vs ->  _getFocused vs ws $ Just seat)

getWSOutputs :: WSTag a => a -> Way vs a [Output]
getWSOutputs ws = do
    mapping <- liftIO . readIORef . wayBindingMapping =<< getState
    pure $ map snd $ filter ((==) ws . fst) mapping

-- | This is a utility function that makes sure things are relayouted/focus is
-- set appropriatly when the modified workspace is displayed
modifyWS :: forall a vs. (WSTag a, FocusCore vs a)
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

    vsRef <- wayBindingState <$> getState
    vs <- liftIO $ readIORef vsRef
    let vs' = fun ws vs
    liftIO $ writeIORef vsRef vs'

    let changed = not $ sameVS (undefined :: a) vs vs'
    pre <- getWSLayout vs ws
    post <- getWSLayout vs' ws

    when (changed && pre /= post) $ do
        unfreeze <- freezeLayout ws vs

        case post of
            ((out, layout):_) -> do
                Point ox oy <- liftIO $ getOutputPosition $ outputRoots out
                doApply <- unliftWay $ mapM_ (uncurry applyLayout) post
                void $ sendLayout (ox, oy) layout (unfreeze >> doApply)
            _ -> pure ()

    mapM_ (\s -> setFocused s Intentional ws) seats

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

setViewsetFocus :: (WSTag ws, FocusCore vs ws) => Seat -> View -> Way vs ws ()
setViewsetFocus seat view = doJust (getPointerOutputS seat) $ \output -> do
    prev <- getKeyboardFocus seat
    ret <- keyboardEnter seat Intentional view
    when ret $ do
        whenJust prev $ flip activateView False
        setSeatOutput seat (That output) Intentional
        modifyCurrentWS $ \_ ws vs -> _focusView ws seat view vs

removeCB :: forall vs ws. (FocusCore vs ws, WSTag ws) => ws -> View -> Way vs ws ()
removeCB ws v = do
    modifyWS ws (\ws' -> _removeView ws' v)

    outputs <- getOutputs
    forM_ outputs $ \output -> do
        seats <- getOutputKeyboards output
        doJust (getOutputWS output) $ \ws' -> do
            mapM_ (\s -> setFocused s SideEffect ws') seats

makeManager :: (FocusCore vs ws, WSTag ws) => ws -> Way vs ws ManagerData
makeManager ws  = do
    focus <- makeCallback2 setViewsetFocus
    remove <- makeCallback $ removeCB ws
    applyDamage <- makeCallback2 $ applyLayerDamage "main"
    getPos <- makeCallback $ getLayerPosition' "main"
    pure $ ManagerData remove focus applyDamage getPos

probeVS :: (FocusCore vs ws, WSTag ws) => View -> ws -> Maybe Seat -> Way vs ws vs
probeVS view ws seat = withViewSet . const $ _insertView ws seat view

insertView :: (FocusCore vs a, WSTag a) => View -> a -> Maybe Seat -> Way vs a ()
insertView v ws s = do
    whenJust s (`unsetFocus` ws)
    setViewManager v =<< makeManager ws
    modifyWS ws (\ws' -> _insertView ws' s v)

removeView :: (FocusCore vs a, WSTag a) => View -> a -> Way vs a ()
removeView v ws = do
    activateView v False
    unsetViewManager v
    modifyWS ws (\ws' -> _removeView ws' v)
