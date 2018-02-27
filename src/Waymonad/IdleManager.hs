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
{-# LANGUAGE LambdaCase #-}
module Waymonad.IdleManager
    ( IdleEvent (..)
    , getIdleBracket
    , idleLog
    , isIdle
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
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

import Waymonad.Start (Bracketed (..))
import Waymonad.Utility.Base (whenJust)
import Waymonad (unliftWay, sendEvent, getEvent)
import Waymonad.Extensible (ExtensionClass (..))
import Waymonad.Types (Way, EventClass, SomeEvent)
import Waymonad.Utility.Extensible (getEState, setEState)
import Waymonad.Utility.Signal (setSignalHandler, setDestroyHandler)

newtype Idle = Idle Bool deriving (Eq, Show)

instance ExtensionClass Idle where
    initialValue = Idle False

data IdleEvent
    = IdleStart
    | IdleStop

instance EventClass IdleEvent

gotInput :: EventSource -> Int -> Way vs a ()
gotInput src msecs = do
    (Idle idle) <- getEState
    if idle
        then do
            sendEvent IdleStop
            setEState $ Idle False
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

idleSetup :: Int -> DisplayServer -> Ptr Backend -> Way vs a ListenerToken
idleSetup msecs dsp backend = do
    evtLoop <- liftIO $ displayGetEventLoop dsp
    cb <- unliftWay (sendEvent IdleStart >> setEState (Idle True))
    src <- liftIO $ eventLoopAddTimer evtLoop (cb >> pure False)

    let signals = backendGetSignals backend
    setSignalHandler (backendEvtInput signals) $ handleInputAdd (gotInput src msecs)

getIdleBracket :: Int -> Bracketed vs (DisplayServer, Ptr Backend) a
getIdleBracket msecs = Bracketed (uncurry (idleSetup msecs)) (const $ pure ())

idleLog :: SomeEvent -> Way vs a ()
idleLog evt = whenJust (getEvent evt) $ \case
    IdleStart -> liftIO $ hPutStrLn stderr "Setting up idle state"
    IdleStop -> liftIO $ hPutStrLn stderr "Tearing down idle state"

isIdle :: Way vs ws Bool
isIdle = (\(Idle x) -> x) <$> getEState
