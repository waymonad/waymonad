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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Managehook
    ( Query
    , runQuery
    , liftWay
    , enactInsert

    , Managehook
    , InsertAction (..)
    , query
    , withView
    , insertView
    , configureView
    , removeView
    )
where

import Control.Monad (forM_, when, void, unless)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), ask, lift)
import Data.List (find)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Waymonad
import Waymonad.Input.Seat
import Waymonad.Layout (getWSLayout)
import Waymonad.Types
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Current (getCurrentWS)
import Waymonad.Utility.Floating
import Waymonad.Utility.Focus (focusView)
import Waymonad.View
import Waymonad.ViewSet

import qualified Waymonad.Utility.ViewSet as VS

liftWay :: Way vs a b -> Query vs a b
liftWay = Query . lift

withView :: (View -> Way vs a b) -> Query vs a b
withView act = liftWay . act =<< ask

runQuery :: View -> Query vs a b -> Way vs a b
runQuery v (Query m) = runReaderT m v

doConfigure :: (FocusCore vs ws, WSTag ws)
            => View -> ws -> vs -> Way vs ws ()
doConfigure view ws vs = do
    layout <- map snd <$> getWSLayout vs ws
    case layout of
        [] -> pure ()
        (x:_) -> case find (\(v, _, _) -> v == view) x of
            Nothing -> pure ()
            Just (_, _, WlrBox _ _ w h) -> void $ resizeView view (fromIntegral w) (fromIntegral h) (pure ())

enactInsert :: (FocusCore vs a, WSTag a)
            => InsertAction vs a -> Bool -> Query vs a ()
enactInsert act dry = do
    view <- ask
    hook <- wayHooksVWSChange . wayCoreHooks <$> liftWay getState
    liftWay $ case act of
        InsertNone -> pure ()
        InsertFocused -> do
            ws <- getCurrentWS
            seat <- getSeat
            case dry of
                False -> do
                    VS.insertView view ws seat
                    focusView view
                    hook $ WSEnter view ws
                True -> do
                    vs <- VS.probeVS view ws seat
                    doConfigure view ws vs
        InsertInto ws -> do
            seat <- getSeat
            case dry of
                False -> do
                    VS.insertView view ws seat
                    hook $ WSEnter view ws
                True -> do
                    vs <- VS.probeVS view ws seat
                    doConfigure view ws vs
        InsertFloating box -> unless dry $ do
            setFloating view box
            doFocus <- viewTakesFocus view SeatKeyboard
            when doFocus $ doJust getSeat $ \seat ->
                void $ keyboardEnter seat SideEffect view
        InsertCustom ins -> ins dry


query :: Query vs a View
query = ask

configureView :: (FocusCore vs a, WSTag a) => View -> Way vs a ()
configureView v = do
    hook <- wayManagehook <$> getState
    runQuery v $ flip enactInsert True . flip mappend InsertFocused =<< hook

insertView :: (FocusCore vs a, WSTag a) => View -> Way vs a ()
insertView v = do
    hook <- wayManagehook <$> getState
    runQuery v $ flip enactInsert False . flip mappend InsertFocused =<< hook

removeView :: forall vs ws. (FocusCore vs ws, WSTag ws)
           => View -> Way vs ws ()
removeView v = do
    seats <- getSeats

    doRemoveView v

    forM_ seats $ \seat -> do
        kFoc <- getKeyboardFocus seat
        when (kFoc == Just v) $ keyboardClear seat
        pFoc <- getPointerFocus seat
        when (pFoc == Just v) $ pointerClear seat
