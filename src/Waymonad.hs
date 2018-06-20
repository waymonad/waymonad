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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Waymonad
    ( get
    , modify

    , KeyBinding
    , BindingMap
    , LogFun

    , WayBindingState (..)

    , Way
    , getState
    , getSeat
    , runWay
    , unliftWay
    , makeCallback
    , makeCallback2
    , makeCallback3
    , setCallback
    , withSeat
    , getViewSet

    , WayLoggers (..)
    , Logger (..)
    , getLoggers

    , EventClass
    , SomeEvent
    , getEvent
    , sendEvent

    , getSeats
    , registerTimed
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..), local)
import Data.IORef (IORef, modifyIORef, readIORef, newIORef, writeIORef)
import Data.Typeable (cast)

import Graphics.Wayland.Server (displayGetEventLoop, eventLoopAddTimer, eventSourceRemove, eventSourceTimerUpdate)

import {-# SOURCE #-} Waymonad.Input.Seat (Seat)
import Waymonad.Types


get :: (MonadReader (IORef a) m, MonadIO m) => m a
get = liftIO . readIORef =<< ask

modify :: (MonadReader (IORef a) m, MonadIO m) => (a -> a) -> m ()
modify fun = do
    ref <- ask
    liftIO $ modifyIORef ref fun

getEvent :: EventClass e => SomeEvent -> Maybe e
getEvent (SomeEvent e) = cast e

sendEvent :: EventClass e => e -> Way vs a ()
sendEvent e = flip wayEventHook (SomeEvent e) =<< getState

getLoggers :: Way vs a WayLoggers
getLoggers = wayLoggers <$> getState

getState :: Way vs a (WayBindingState vs a)
getState = ask

getSeat :: Way vs a (Maybe Seat)
getSeat = do
    current <- wayCurrentSeat <$> getState
    case current of
        Just _ -> pure current
        Nothing -> do
            seats <- liftIO . readIORef . wayBindingSeats =<< getState
            pure $ case seats of
                [x] -> Just x
                _ -> Nothing

getViewSet :: Way vs a vs
getViewSet = liftIO . readIORef . wayBindingState =<< getState

unliftWay :: Way vs a b -> Way vs a (IO b)
unliftWay act = do
    state <- getState
    pure $ runWay state act

makeCallback :: (c -> Way vs a b) -> Way vs a (c -> IO b)
makeCallback act = do
    state <- getState
    pure $ runWay state . act

makeCallback2 :: (c -> d -> Way vs a b) -> Way vs a (c -> d -> IO b)
makeCallback2 act = curry <$> makeCallback (uncurry act)

makeCallback3 :: (c -> d -> e -> Way vs a b) -> Way vs a (c -> d -> e -> IO b)
makeCallback3 act = curry . curry <$> makeCallback (uncurry $ uncurry act)

setCallback :: (c -> Way vs a b) -> ((c -> IO b) -> IO d) -> Way vs a d
setCallback act fun = do
    ioAct <- makeCallback act
    liftIO $ fun ioAct


withSeat :: Maybe Seat -> Way vs a b -> Way vs a b
withSeat seat (Way m) = Way $ local (\s -> s {wayCurrentSeat = seat}) m

getSeats :: Way vs a [Seat]
getSeats = liftIO . readIORef . wayBindingSeats =<< getState

registerTimed :: Way vs ws () -> Int -> Way vs ws ()
registerTimed act ms = do
    WayBindingState {wayCompositor = comp} <- getState
    let dsp = compDisplay comp
    unlifted <- unliftWay act
    liftIO $ do
        loop <- displayGetEventLoop dsp
        evtRef <- newIORef $ error "Tried to access the timer eventsource IORef to early"
        source <- eventLoopAddTimer loop $ do
            unlifted
            eventSourceRemove =<< readIORef evtRef
            pure False
        writeIORef evtRef source
        void $ eventSourceTimerUpdate source ms
