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
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.Server
    ( DisplayServer
    , displayTerminate
    )
import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    )
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

    , KeyboardModifiers (..)
    , readModifiers

    , getModifiers
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
    )
import Control.Monad (forM, when)

import Input.Seat
import ViewSet (WSTag)
import Waymonad
    ( BindingMap
    , withSeat
    , Way
    , WayLoggers (..)
    )
import WayUtil (setSignalHandler, logPutText)

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
    :: WSTag a
    => DisplayServer
    -> Ptr Backend
    -> BindingMap a
    -> Word32
    -> Keysym
    -> Way a Bool
handleKeyPress dsp backend bindings modifiers sym@(Keysym key) = do
    case sym of
        Keysym_e -> when (modifiers == 9) (liftIO (displayTerminate dsp)) >> pure False
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
                    logPutText loggerKeybinds "Found a keybind"
                    fun
                    pure True


tellClient :: Seat -> Keyboard -> EventKey -> IO ()
tellClient seat keyboard event = do
    seatSetKeyboard   (seatRoots seat) $ keyboardIDevice keyboard
    keyboardNotifyKey (seatRoots seat) (timeSec event) (keyCode event) (state event)

handleKeySimple
    :: WSTag a
    => DisplayServer
    -> Ptr Backend
    -> BindingMap a
    -> Keyboard
    -> CKeycode
    -> Way a Bool
handleKeySimple dsp backend bindings keyboard keycode = do
    keystate <- liftIO . getKeystate $ keyboardDevice keyboard
    keymap   <- liftIO . getKeymap $ keyboardDevice keyboard
    modifiers <- liftIO $ getModifiers $ keyboardDevice keyboard

    layoutL <- liftIO $ keyGetLayoutI keystate keycode
    syms <- liftIO $ keymapSymsByLevelI keymap keycode layoutL (CLevelIndex 0)

    handled <- forM syms $
        handleKeyPress dsp backend bindings modifiers

    pure $ foldr (||) False handled

handleKeyXkb
    :: WSTag a
    => DisplayServer
    -> Ptr Backend
    -> BindingMap a
    -> Keyboard
    -> CKeycode
    -> Way a Bool
handleKeyXkb dsp backend bindings keyboard keycode = do
    keystate <- liftIO . getKeystate $ keyboardDevice keyboard
    modifiers <- liftIO $ getModifiers $ keyboardDevice keyboard
    consumed <- liftIO $ keyGetConsumedMods2 keystate keycode

    let usedMods = modifiers .&. complement (fromIntegral consumed)

    syms <- liftIO $ getStateSymsI keystate keycode

    handled <- forM syms $
        handleKeyPress dsp backend bindings usedMods

    pure $ foldr (||) False handled


handleKeyEvent
    :: WSTag a
    => DisplayServer
    -> Ptr Backend
    -> Keyboard
    -> Seat
    -> BindingMap a
    -> Ptr EventKey
    -> Way a ()
handleKeyEvent dsp backend keyboard seat bindings ptr = withSeat (Just seat) $ do
    event <- liftIO $ peek ptr
    let keycode = fromEvdev . fromIntegral . keyCode $ event

    handled <- case (state event) of
        -- We currently don't do anything special for releases
        KeyReleased -> pure False
        KeyPressed -> do
            handled <- handleKeyXkb dsp backend bindings keyboard keycode
            if handled
                then pure handled
                else handleKeySimple dsp backend bindings keyboard keycode

    liftIO . when (not handled) $ tellClient seat keyboard event

handleModifiers :: Keyboard -> Seat -> Ptr a -> IO ()
handleModifiers keyboard seat _ = do
    mods <- readModifiers $ keyboardDevice keyboard
    seatSetKeyboard (seatRoots seat) $ keyboardIDevice keyboard
    keyboardNotifyModifiers (seatRoots seat) (modDepressed mods) (modLatched mods) (modLocked mods) (modGroup mods)

handleKeyboardAdd
    :: WSTag a
    => DisplayServer
    -> Ptr Backend
    -> Seat
    -> BindingMap a
    -> Ptr InputDevice
    -> Ptr WlrKeyboard
    -> Way a ()
handleKeyboardAdd dsp backend seat bindings dev ptr = do
    let signals = getKeySignals ptr

    liftIO $ do
        (Just cxt) <- newContext defaultFlags
        (Just keymap) <- newKeymapFromNamesI cxt noPrefs
        setKeymap ptr keymap

    let keyboard = Keyboard ptr dev

    kh <- setSignalHandler
        (keySignalKey signals)
        (handleKeyEvent dsp backend keyboard seat bindings)
    mh <- liftIO $ addListener (WlListener $ handleModifiers keyboard seat) (keySignalModifiers signals)

    liftIO $ do
        sptr <- newStablePtr (kh, mh)
        poke (getKeyDataPtr ptr) (castStablePtrToPtr sptr)


handleKeyboardRemove :: Ptr WlrKeyboard -> IO ()
handleKeyboardRemove ptr = do
    sptr <- peek (getKeyDataPtr ptr)
    freeStablePtr $ castPtrToStablePtr sptr
