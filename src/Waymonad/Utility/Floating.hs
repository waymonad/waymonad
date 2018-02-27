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
    )
where

import Control.Monad (when, void, forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)
import Data.Set (Set)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Waymonad.Input.Seat (Seat, keyboardEnter)
import Waymonad.Output
import Waymonad.Utility.Base (doJust)
import Waymonad.View
    ( View, moveView, resizeView
    , setViewManager, unsetViewManager
    , activateView, doFocusView
    )
import Waymonad.ViewSet (WSTag, FocusCore (..))
import Waymonad
    ( Way
    , makeCallback
    , makeCallback2
    , getSeat
    )
import Waymonad.Extensible
import Waymonad.Types (SSDPrio (NoSSD), EvtCause (Intentional))
import Waymonad.Types.Core (ManagerData (..))
import Waymonad.Utility (getOutputs)
import Waymonad.Utility.Current (getCurrentBox, getCurrentView, getCurrentWS)
import Waymonad.Utility.Extensible (modifyEState, getEState)
import Waymonad.Utility.LayerCache (applyLayerDamage, getLayerPosition')
import Waymonad.Utility.ViewSet (modifyFocusedWS, insertView)

import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace

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

makeFloatManager :: Way vs ws ManagerData
makeFloatManager = do
    focus <- makeCallback2 focusFloating
    remove <-  makeCallback removeFloating
    applyDamage <- makeCallback2 $ applyLayerDamage "floating"
    getPos <- makeCallback $ getLayerPosition' "floating"
    pure $ ManagerData remove focus applyDamage getPos

setFloating :: View -> WlrBox -> Way vs a ()
setFloating view pos@(WlrBox x y width height) = do
    moveView view (fromIntegral x) (fromIntegral y)
    resizeView view (fromIntegral width) (fromIntegral height)
    modifyFloating $ S.insert view
    setViewManager view =<< makeFloatManager

    outputs <- getOutputs
    forM_ outputs $ \output -> do
        intersects <- intersectsOutput (traceShowId output) pos
        when intersects $ do
            doJust (traceShowId <$> getOutputBox output) $ \(WlrBox ox oy _ _) -> do
                let viewBox = WlrBox (x - ox) (y - oy) width height
                let ref = (M.!) (outputLayers output) "floating"
                liftIO $ modifyIORef ref ((view, NoSSD mempty, viewBox):)

removeFloating :: View -> Way vs ws ()
removeFloating view = do
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
