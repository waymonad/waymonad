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
module Waymonad.Protocols.InputInhibit
    ( getInputInhibitBracket
    , getInhibitingClient
    , isInhibited
    )
where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, writeIORef, readIORef)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Resource (resourceGetClient)
import Graphics.Wayland.Server (DisplayServer, Client)
import Graphics.Wayland.Signal (ListenerToken, removeListener)
import Graphics.Wayland.WlRoots.InputInhibitor
import Graphics.Wayland.WlRoots.Surface (WlrSurface, getSurfaceResource)

import Waymonad (getState, getSeats)
import Waymonad.GlobalFilter (registerGlobal)
import Waymonad.Start (Bracketed (..))
import Waymonad.Input.Seat (getPointerFocus, getKeyboardFocus, pointerClear, keyboardClear)
import Waymonad.Types (Way, WayBindingState (..))
import Waymonad.Utility.Base (doJust)
import Waymonad.Utility.Signal (setSignalHandler)
import Waymonad.View (getViewClient)

isInhibited :: Ptr WlrSurface -> Way vs ws Bool
isInhibited surf = do
    inhib <- getInhibitingClient
    case inhib of
        Just inhibC -> do
            client <- liftIO (resourceGetClient =<< getSurfaceResource surf)
            pure $ inhibC /= client
        Nothing -> pure False

getInhibitingClient :: Way vs ws (Maybe Client)
getInhibitingClient = liftIO . readIORef =<< getRef

getRef :: Way vs ws (IORef (Maybe Client))
getRef = wayBindingLock <$> getState

activate :: Ptr WlrInputInhibitor -> Way vs ws ()
activate ptr = do
    let manager = WlrInputInhibitor ptr
    client <- liftIO $ getInhibitClient manager
    ref <- getRef
    liftIO $ writeIORef ref client

    seats <- getSeats
    forM_ seats $ \seat -> do
        doJust (getPointerFocus seat) $ \pv -> do
            cli <- getViewClient pv
            when (cli /= client) $ pointerClear seat
        doJust (getKeyboardFocus seat) $ \kv -> do
            cli <- getViewClient kv
            when (cli /= client) $ keyboardClear seat

deactivate :: Ptr WlrInputInhibitor -> Way vs ws ()
deactivate ptr = do
    let manager = WlrInputInhibitor ptr
    client <- liftIO $ getInhibitClient manager
    ref <- getRef
    liftIO $ writeIORef ref client

makeManager :: DisplayServer -> Way vs ws (WlrInputInhibitor, ListenerToken, ListenerToken)
makeManager display = do
    ret <- liftIO $ createInputInhibitor display
    registerGlobal "InputInhibitorv1" =<< liftIO (getInputInhibitGlobal ret)
    let signals = getInputInhibitorEvents ret
    active <- setSignalHandler (inputInhibitEventsActivate signals) activate
    deactive <- setSignalHandler (inputInhibitEventsDeactivate signals) deactivate
    pure (ret, active, deactive)

destroyManager :: (WlrInputInhibitor, ListenerToken, ListenerToken) -> Way vs ws ()
destroyManager (manager, t1, t2) = liftIO $ do
    removeListener t1
    removeListener t2
    destroyInputInhibitor manager

getInputInhibitBracket :: Bracketed vs DisplayServer ws
getInputInhibitBracket = Bracketed makeManager destroyManager
