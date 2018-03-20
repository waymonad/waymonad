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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Waymonad.IdleManager
    ( IdleEvent (..)
    , getIdleBracket
    , getIdleBracket'
    , idleLog
    , isIdle
    , setIdleTime
    , idleIPC
    , idleHandler
    )
where

import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)

import System.IO

import Graphics.Wayland.Signal (ListenerToken, removeListener)
import Graphics.Wayland.Server
    ( DisplayServer
    , displayGetEventLoop
    , eventLoopAddTimer
    , eventSourceTimerUpdate
    , EventSource
    )

import Graphics.Wayland.WlRoots.Backend (Backend, backendGetSignals, BackendSignals (..))
import Graphics.Wayland.WlRoots.Input (InputDevice, inputDeviceType, getDestroySignal, DeviceType (..))
import Graphics.Wayland.WlRoots.Input.Pointer (WlrPointer, PointerEvents (..), pointerGetEvents)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrKeyboard, KeyboardSignals (..), getKeySignals)

import Waymonad.IPC
import Waymonad.Start (Bracketed (..))
import Waymonad (unliftWay, sendEvent, getEvent)
import Waymonad.Extensible (ExtensionClass (..))
import Waymonad.Types (Way, EventClass, SomeEvent)
import Waymonad.Utility.Base (whenJust)
import Waymonad.Utility.Extensible (getEState, setEState)
import Waymonad.Utility.Signal (setSignalHandler, setDestroyHandler)

newtype Idle a = Idle Bool deriving (Eq, Show)

instance Typeable a => ExtensionClass (Idle a) where
    initialValue = Idle False

data IdleStore a = IdleStore !Int !(Maybe EventSource)

instance Typeable a => ExtensionClass (IdleStore a) where
    initialValue = IdleStore 0 Nothing

data IdleEvent = IdleStart | IdleStop

instance EventClass IdleEvent

gotInput :: forall a ws vs. Typeable a => Proxy a -> EventSource -> Way vs ws ()
gotInput _ src = do
    (IdleStore msecs _) :: IdleStore a <- getEState
    (Idle idle) :: Idle a <- getEState
    if idle
        then do
            sendEvent IdleStop
            setEState $ (Idle False :: Idle a)
        else void . liftIO $ eventSourceTimerUpdate src msecs

handlePointerAdd :: Way vs a () -> Ptr WlrPointer -> Way vs a [ListenerToken]
handlePointerAdd report ptr = do
    let events = pointerGetEvents ptr
    sequence    [ setSignalHandler (pointerButton events) (const report)
                , setSignalHandler (pointerMotion events) (const report)
                , setSignalHandler (pointerMotionAbs events) (const report)
                , setSignalHandler (pointerAxis events) (const report)
                ]

handleKeyboardAdd :: Way vs a () -> Ptr WlrKeyboard -> Way vs a [ListenerToken]
handleKeyboardAdd report ptr = do
    let events = getKeySignals ptr
    sequence    [ setSignalHandler (keySignalKey events) (const report) ]

handleInputAdd :: Way vs a () -> Ptr InputDevice -> Way vs a ()
handleInputAdd report ptr = do
    iType <- liftIO $ inputDeviceType ptr
    listeners <- case iType of
        (DeviceKeyboard kptr) -> handleKeyboardAdd report kptr
        (DevicePointer pptr) -> handlePointerAdd report pptr
        _ -> pure []

    setDestroyHandler (getDestroySignal ptr) (const . liftIO $ mapM_ removeListener listeners)

idleSetup :: forall a vs ws. Typeable a => Proxy a -> Way vs ws () -> Int -> DisplayServer -> Ptr Backend -> Way vs ws ListenerToken
idleSetup proxy act msecs dsp backend = do
    evtLoop <- liftIO $ displayGetEventLoop dsp
    cb <- unliftWay (act >> setEState (Idle True :: Idle a))
    src <- liftIO $ eventLoopAddTimer evtLoop (cb >> pure False)

    setEState $ ((IdleStore msecs $ Just src) :: IdleStore a)
    gotInput proxy src

    let signals = backendGetSignals backend
    setSignalHandler (backendEvtInput signals) $ handleInputAdd (gotInput proxy src)

-- | More generic voersion of 'getIdleBracket' in case more than one timer is
-- required. Does not go over event system, but has to be directly wired up
getIdleBracket' :: Typeable a => Proxy a -> Way vs ws () -> Int -> Bracketed vs (DisplayServer, Ptr Backend) ws
getIdleBracket' proxy act msecs = Bracketed (uncurry (idleSetup proxy act msecs)) (const $ pure ())

-- | Send an event after @argument@ milli seconds of no input on any input
-- device.
getIdleBracket :: Int -> Bracketed vs (DisplayServer, Ptr Backend) ws
getIdleBracket = getIdleBracket' (Proxy :: Proxy IdleEvent) (sendEvent IdleStart)

idleLog :: SomeEvent -> Way vs a ()
idleLog = idleHandler (liftIO $ hPutStrLn stderr "Setting up idle state") (liftIO $ hPutStrLn stderr "Tearing down idle state")

isIdle :: forall a vs ws. Typeable a => Proxy a -> Way vs ws Bool
isIdle _ = (\(Idle x :: Idle a) -> x) <$> getEState

setIdleTime :: forall a vs ws. Typeable a => Proxy a -> Int -> Way vs ws ()
setIdleTime proxy msecs = do
    IdleStore _ src :: IdleStore a <- getEState
    idles <- isIdle proxy
    liftIO $ unless idles $ whenJust src $ void . flip eventSourceTimerUpdate msecs
    setEState $ (IdleStore msecs src :: IdleStore a)

getIdleTime :: forall a vs ws. Typeable a => Proxy a -> Way vs ws Int
getIdleTime _ = fmap (\(IdleStore x _ :: IdleStore a) -> x) getEState 

idleHandler :: Way vs ws () -> Way vs ws () -> SomeEvent -> Way vs ws ()
idleHandler start stop evt = whenJust (getEvent evt) $ \case
    IdleStart -> start
    IdleStop -> stop


idleIPC :: forall a vs ws. Typeable a => Proxy a -> IPCEntry vs ws
idleIPC proxy = IPCEntry
    { ipcEntryRead  = [simpleIPCRead $ getIdleTime proxy, textifyIPCRead $ getIdleTime proxy]
    , ipcEntryWrite = [simpleIPCWrite $ setIdleTime proxy , textifyIPCWrite $ setIdleTime proxy]
    , ipcEntryReadable  = isJust . (\(IdleStore _ src :: IdleStore a) -> src) <$> getEState
    , ipcEntryWriteable = isJust . (\(IdleStore _ src :: IdleStore a) -> src) <$> getEState
    }

