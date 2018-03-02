{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2018  Markus Ongyerth

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
module Waymonad.Protocols.IdleInhibit
    ( makeManager
    , getIdleInihibitBracket
    , getInhibitedOutputs
    , IdleInhibitChange
    )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (for)
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (nub)
import Data.Set (Set)

import Graphics.Wayland.Signal (ListenerToken, removeListener)
import Graphics.Wayland.Server (DisplayServer)
import Graphics.Wayland.WlRoots.IdleInhibit

import Waymonad (sendEvent)
import Waymonad.Extensible
import Waymonad.GlobalFilter
import Waymonad.Output (outputFromWlr)
import Waymonad.Start (Bracketed (..))
import Waymonad.Types (Way, Output, EventClass)
import Waymonad.Types.Core (View)
import Waymonad.Utility.Extensible
import Waymonad.Utility.Signal (setDestroyHandler, setSignalHandler)
import Waymonad.View (getViewFromSurface, doGetPosition)

import qualified Data.Set as S

newtype IdleInhibitors = IdleInhibitors { unIIS :: Set View }
newtype Unmatched = Unmatched { unUM :: Set IdleInhibitor }

instance ExtensionClass IdleInhibitors where
    initialValue = IdleInhibitors mempty

instance ExtensionClass Unmatched where
    initialValue = Unmatched mempty

data IdleInhibitChange = IdleInhibitChange deriving (Eq, Show)

instance EventClass IdleInhibitChange

makeManager :: DisplayServer -> Way vs ws (IdleInhibitManager, ListenerToken)
makeManager display = do
    ptr <- liftIO $ idleInhibitCreate display
    registerGlobal "IdleInhibitv1" =<< liftIO (getIdleInhibitGlobal ptr)
    let signal = getIdleInhibitSignal ptr
    token <- setSignalHandler signal (addIdleView . IdleInhibitor)
    pure (ptr, token)


getIdleInihibitBracket :: Bracketed vs DisplayServer ws
getIdleInihibitBracket = Bracketed makeManager (\(manager, token) -> liftIO $ do
    removeListener token
    idleInhibitDestroy manager
                                               )

addIdleView :: IdleInhibitor -> Way vs ws ()
addIdleView inhibitor = do
    let signal = getInhibitorDestroy inhibitor
    surface <- liftIO $ getInhibitorSurface inhibitor
    viewM <- getViewFromSurface surface
    case viewM of
        (Just view) -> do
            modifyEState (IdleInhibitors . S.insert view . unIIS)
            setDestroyHandler signal $ \_ -> destroyIdleInhibitor view
            sendEvent IdleInhibitChange
        Nothing -> do
            modifyEState (Unmatched . S.insert inhibitor . unUM)
            setDestroyHandler signal $ \_ -> modifyEState (Unmatched . S.delete inhibitor . unUM)


destroyIdleInhibitor :: View -> Way vs ws ()
destroyIdleInhibitor view = do
    modifyEState (IdleInhibitors . S.delete view . unIIS)
    sendEvent IdleInhibitChange

ensureInhibitors :: Way vs ws ()
ensureInhibitors = do
    ums <- unUM <$> getEState
    ret <- for (S.toList ums) $ \inhib -> do
        surface <- liftIO $ getInhibitorSurface inhib
        viewM <- getViewFromSurface surface
        case viewM of
            Nothing -> pure $ Left inhib
            Just view -> do
                let signal = getInhibitorDestroy inhib
                modifyEState (IdleInhibitors . S.insert view . unIIS)
                setDestroyHandler signal $ \_ -> destroyIdleInhibitor view
                pure $ Right view

    let newUm = mapMaybe (\case Left x -> Just x; Right _ -> Nothing) ret
    let newVs = mapMaybe (\case Right x -> Just x; Left _ -> Nothing) ret

    setEState (Unmatched $ S.fromList newUm)
    unless (null newVs) $ do
        modifyEState (IdleInhibitors . S.union (S.fromList newVs) . unIIS)
        sendEvent IdleInhibitChange


getInhibitedOutputs :: Way vs ws (Set Output)
getInhibitedOutputs = do
    ensureInhibitors
    views <- S.toList . unIIS <$> getEState
    xs <- mapM doGetPosition views
    let wlrs = nub $ fmap fst $ concat xs
    fmap (S.fromList . catMaybes) $ mapM outputFromWlr wlrs
