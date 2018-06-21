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
module Waymonad.Utility.Floating
    ( centerFloat
    , toggleFloat
    , isFloating
    , getFloats
    , modifyFloating
    , unsetFloating
    , setFloating
    , moveFloat
    , resizeFloat
    , flattenView
    )
where

import Control.Monad (when, void, forM_, unless, filterM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)
import Data.Set (Set)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Waymonad.Input.Seat (keyboardEnter, getKeyboardFocus)
import Waymonad.Output
import Waymonad.Utility.Base (doJust)
import Waymonad.View
    ( View, moveView, resizeView, updateViewSize
    , setViewManager, unsetViewManager, setViewSize
    , activateView, doFocusView, getViewSize
    , addViewResizeListener
    )
import Waymonad.ViewSet (WSTag, FocusCore (..))
import Waymonad
    ( Way
    , makeCallback, makeCallback2
    , getSeat, getSeats, withSeat
    )
import Waymonad.Extensible
import Waymonad.Types (SSDPrio (NoSSD), EvtCause (..))
import Waymonad.Types.Core (ManagerData (..), Seat)
import Waymonad.Utility (getOutputs)
import Waymonad.Utility.Current (getCurrentBox, getCurrentView, getCurrentWS)
import Waymonad.Utility.Extensible (modifyEState, getEState)
import Waymonad.Utility.HaskellSignal (HaskellSignalToken, removeHaskellListener)
import Waymonad.Utility.LayerCache (applyLayerDamage, getLayerPosition')
import Waymonad.Utility.ViewSet (modifyFocusedWS, insertView, getFocused)

import qualified Data.Set as S
import qualified Data.Map as M

newtype FSet = FSet {unFS:: Set View}

instance ExtensionClass FSet where
    initialValue = FSet mempty


modifyFloating :: (Set View -> Set View) -> Way vs a ()
modifyFloating fun = modifyEState (FSet . fun . unFS)


getFloats :: Way vs a (Set View)
getFloats = unFS <$> getEState


isFloating :: View -> Way vs a Bool
isFloating v = S.member v <$> getFloats


focusFloating :: Seat -> View -> Way vs ws ()
focusFloating seat view = do
    activateView view True
    void $ keyboardEnter seat Intentional view


makeFloatManager :: FocusCore vs ws => HaskellSignalToken View IO -> Way vs ws ManagerData
makeFloatManager token = do
    focus <- makeCallback2 focusFloating
    remove <-  makeCallback $ \view -> do
        removeHaskellListener token
        removeFloating view
    applyDamage <- makeCallback2 $ applyLayerDamage "floating"
    getPos <- makeCallback $ getLayerPosition' "floating"
    pure $ ManagerData remove focus applyDamage getPos


handleViewResize :: View -> Way vs ws ()
handleViewResize view = do
    (width, height) <- getViewSize view
    updateViewSize view (floor width) (floor height)
    outputs <- getOutputs
    forM_ outputs $ \output -> liftIO $ do
        let ref = (M.!) (outputLayers output) "floating"

        modifyIORef ref $ fmap (\(v, d, b) -> if v == view
            then (v, d, b)
            else (v, d, b { boxWidth = floor width, boxHeight = floor height })
            )


setFloating :: FocusCore vs ws => View -> WlrBox -> Way vs ws ()
setFloating view pos@(WlrBox x y width height) = do
    moveView view x y
    _ <- resizeView view (fromIntegral width) (fromIntegral height) (pure ())
    modifyFloating $ S.insert view

    resizeCB <- makeCallback handleViewResize
    setViewManager view =<< makeFloatManager  =<< addViewResizeListener resizeCB view

    outputs <- getOutputs
    forM_ outputs $ \output -> do
        intersects <- intersectsOutput output pos
        when intersects $ doJust (getOutputBox output) $ \(WlrBox ox oy _ _) -> do
            let viewBox = WlrBox (x - ox) (y - oy) width height
            let ref = (M.!) (outputLayers output) "floating"
            liftIO $ modifyIORef ref ((view, NoSSD mempty, viewBox):)


flattenView :: FocusCore vs ws => View -> Way vs ws ()
flattenView view = do
    seats <- getSeats
    affected <- filterM (fmap (Just view ==) . getKeyboardFocus) seats
    mapM_ flattenSeat affected


flattenSeat :: FocusCore vs ws => Seat -> Way vs ws ()
flattenSeat seat = {-doJust-} (>>=) (withSeat (Just seat) getCurrentWS) $ \ws ->
    doJust (getFocused seat ws) $ \view -> void $
        keyboardEnter seat SideEffect view


removeFloating :: FocusCore vs ws => View -> Way vs ws ()
removeFloating view = do
    flattenView view
    modifyFloating $ S.delete view
    outputs <- getOutputs
    forM_ outputs $ \output -> liftIO $ do
        let ref = (M.!) (outputLayers output) "floating"
        boxes <- filter (\(v, _, _) -> view == v) <$> readIORef ref
        unless (null boxes) $ outApplyDamage output Nothing
        liftIO $ modifyIORef ref (filter (\(v, _, _) -> view /= v))


unsetFloating :: (WSTag a, FocusCore vs a) => View -> Way vs a ()
unsetFloating view = do
    floats <- isFloating view
    when floats $ do
        unsetViewManager view
        removeFloating view
        ws <- getCurrentWS
        seat <- getSeat
        insertView view ws seat


toggleFloat :: (WSTag a, FocusCore vs a) => WlrBox ->  Way vs a ()
toggleFloat box = doJust getCurrentView $ \view -> do
    floats <- isFloating view
    if floats
        then unsetFloating view
        else do
            modifyFocusedWS (\_ ws vs -> _removeView ws view vs)
            setFloating view box
            doJust getSeat $ doFocusView view


centerFloat :: (WSTag a, FocusCore vs a) => Way vs a ()
centerFloat = doJust getCurrentBox $ \(WlrBox x y w h) -> do
    let nw = w `div` 2
    let nh = h `div` 2
    toggleFloat $ WlrBox (x + nw `div` 2) (y + nh `div` 2) nw nh


moveFloat :: (WSTag ws, FocusCore vs ws) => View -> Int -> Int -> Way vs ws ()
moveFloat view x y = do
    floats <- isFloating view
    when floats $ do
        (w, h) <- getViewSize view
        removeFloating view
        setFloating view (WlrBox x y (floor w) (floor h))


resizeFloat :: (WSTag ws, FocusCore vs ws) => View -> Int -> Int -> Way vs ws ()
resizeFloat view w h = do
    floats <- isFloating view
    when floats $ void $ setViewSize view w h (pure ())
