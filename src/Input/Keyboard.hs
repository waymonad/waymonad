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
module Input.Keyboard
where

import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), complement)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.Signal (removeListener)
import Graphics.Wayland.WlRoots.Backend.Multi (getSession')
import Graphics.Wayland.WlRoots.Backend.Session (changeVT)
import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Keyboard
    ( WlrKeyboard
    , KeyboardSignals (..)
    , getKeySignals
    , getKeyDataPtr
    , EventKey (..)
    , KeyState (..)
    , setKeymap
    , getKeystate
    , getKeymap

    , getModifiers
    , getModifierPtr
    )
import Graphics.Wayland.WlRoots.Seat
    ( seatSetKeyboard
    , keyboardNotifyKey
    , seatSetKeyboard
    , keyboardNotifyModifiers
    )
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    , deRefStablePtr
    )
import Control.Monad (forM, when, unless)

import Input.Seat
import Waymonad
    ( BindingMap
    , withSeat
    , Way
    , WayLoggers (..)
    , getState
    )
import Waymonad.Types
    ( Compositor (compBackend)
    , WayBindingState (wayCompositor, wayKeybinds)
    )
import WayUtil.Signal (setSignalHandler)
import WayUtil.Log (logPutText, LogPriority (..))

import Text.XkbCommon.Context
import Text.XkbCommon.KeyboardState
import Text.XkbCommon.KeycodeList
import Text.XkbCommon.Keymap
import Text.XkbCommon.KeysymPatterns
import Text.XkbCommon.Types

import qualified Data.Map as M

data Keyboard = Keyboard
    { keyboardDevice :: Ptr WlrKeyboard
    , keyboardIDevice :: Ptr InputDevice
    }

keyStateToDirection :: KeyState -> Direction
keyStateToDirection KeyReleased = keyUp
keyStateToDirection KeyPressed  = keyDown


switchVT :: Ptr Backend -> Word -> IO ()
switchVT backend vt = do
    mSession <- getSession' backend
    case mSession of
        Nothing -> pure ()
        Just s -> changeVT s vt


handleKeyPress
    :: BindingMap vs a
    -> Word32
    -> Keysym
    -> Way vs a Bool
handleKeyPress bindings modifiers sym@(Keysym key) = do
    backend <- compBackend . wayCompositor <$> getState
    case sym of
        -- Would be cooler if this wasn't a listing of VTs (probably TH)
        Keysym_XF86Switch_VT_1  -> liftIO (switchVT backend 1 ) >> pure True
        Keysym_XF86Switch_VT_2  -> liftIO (switchVT backend 2 ) >> pure True
        Keysym_XF86Switch_VT_3  -> liftIO (switchVT backend 3 ) >> pure True
        Keysym_XF86Switch_VT_4  -> liftIO (switchVT backend 4 ) >> pure True
        Keysym_XF86Switch_VT_5  -> liftIO (switchVT backend 5 ) >> pure True
        Keysym_XF86Switch_VT_6  -> liftIO (switchVT backend 6 ) >> pure True
        Keysym_XF86Switch_VT_7  -> liftIO (switchVT backend 7 ) >> pure True
        Keysym_XF86Switch_VT_8  -> liftIO (switchVT backend 8 ) >> pure True
        Keysym_XF86Switch_VT_9  -> liftIO (switchVT backend 9 ) >> pure True
        Keysym_XF86Switch_VT_10 -> liftIO (switchVT backend 10) >> pure True
        Keysym_XF86Switch_VT_11 -> liftIO (switchVT backend 11) >> pure True
        Keysym_XF86Switch_VT_12 -> liftIO (switchVT backend 12) >> pure True
        _ -> case M.lookup (modifiers, key) bindings of
                Nothing -> pure False
                Just fun -> do
                    logPutText loggerKeybinds Debug "Found a keybind"
                    fun
                    pure True


tellClient :: Seat -> Keyboard -> EventKey -> IO ()
tellClient seat keyboard event = do
    seatSetKeyboard   (seatRoots seat) $ keyboardIDevice keyboard
    keyboardNotifyKey (seatRoots seat) (timeSec event) (keyCode event) (state event)

handleKeySimple
    :: BindingMap vs a
    -> Keyboard
    -> CKeycode
    -> Way vs a Bool
handleKeySimple bindings keyboard keycode = do
    keystate <- liftIO . getKeystate $ keyboardDevice keyboard
    keymap   <- liftIO . getKeymap $ keyboardDevice keyboard
    modifiers <- liftIO $ getModifiers $ keyboardDevice keyboard

    layoutL <- liftIO $ keyGetLayoutI keystate keycode
    syms <- liftIO $ keymapSymsByLevelI keymap keycode layoutL (CLevelIndex 0)

    handled <- forM syms $
        handleKeyPress bindings modifiers

    pure $ or handled

handleKeyXkb
    :: BindingMap vs a
    -> Keyboard
    -> CKeycode
    -> Way vs a Bool
handleKeyXkb bindings keyboard keycode = do
    keystate <- liftIO . getKeystate $ keyboardDevice keyboard
    modifiers <- liftIO $ getModifiers $ keyboardDevice keyboard
    consumed <- liftIO $ keyGetConsumedMods2 keystate keycode

    let usedMods = modifiers .&. complement (fromIntegral consumed)

    syms <- liftIO $ getStateSymsI keystate keycode

    handled <- forM syms $
        handleKeyPress bindings usedMods

    pure $ or handled

handleKeyEvent
    :: Keyboard
    -> Seat
    -> BindingMap vs a
    -> Ptr EventKey
    -> Way vs a ()
handleKeyEvent keyboard seat bindings ptr = withSeat (Just seat) $ do
    event <- liftIO $ peek ptr
    let keycode = fromEvdev . fromIntegral . keyCode $ event

    handled <- case state event of
        -- We currently don't do anything special for releases
        KeyReleased -> pure False
        KeyPressed -> do
            handled <- handleKeyXkb bindings keyboard keycode
            if handled
                then pure handled
                else handleKeySimple bindings keyboard keycode

    liftIO . unless handled $ tellClient seat keyboard event

handleModifiers :: Keyboard -> Seat -> Ptr a -> Way vs b ()
handleModifiers keyboard seat _ = liftIO $ do
    seatSetKeyboard (seatRoots seat) $ keyboardIDevice keyboard

    keyboardNotifyModifiers (seatRoots seat) (getModifierPtr $ keyboardDevice keyboard)

handleKeyboardAdd
    :: Seat
    -> Ptr InputDevice
    -> Ptr WlrKeyboard
    -> Way vs a ()
handleKeyboardAdd seat dev ptr = do
    let signals = getKeySignals ptr
    bindings <- wayKeybinds <$> getState

    liftIO $ do
        (Just cxt) <- newContext defaultFlags
        (Just keymap) <- newKeymapFromNamesI cxt noPrefs
        setKeymap ptr keymap

    let keyboard = Keyboard ptr dev

    kh <- setSignalHandler
        (keySignalKey signals)
        (handleKeyEvent keyboard seat bindings)
    mh <- setSignalHandler
        (keySignalModifiers signals)
        (handleModifiers keyboard seat)

    liftIO $ do
        sptr <- newStablePtr (kh, mh)
        poke (getKeyDataPtr ptr) (castStablePtrToPtr sptr)


detachKeyboard :: Ptr WlrKeyboard -> IO ()
detachKeyboard ptr = do
    handleKeyboardRemove ptr
    poke (getKeyDataPtr ptr) nullPtr

handleKeyboardRemove :: Ptr WlrKeyboard -> IO ()
handleKeyboardRemove ptr = do
    dptr <- peek (getKeyDataPtr ptr)
    when (dptr /= nullPtr) (do
        let sptr = castPtrToStablePtr dptr
        (kh, mh) <- deRefStablePtr sptr
        removeListener kh
        removeListener mh
        freeStablePtr sptr
                           )
