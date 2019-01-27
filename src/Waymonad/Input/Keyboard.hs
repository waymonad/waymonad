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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Waymonad.Input.Keyboard
where

import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), complement)
import Data.IORef (readIORef, modifyIORef)
import Data.Maybe (isJust)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.Signal (removeListener)
import Graphics.Wayland.WlRoots.Backend.Session (changeVT)
import Graphics.Wayland.WlRoots.Backend (Backend, getSession)
import Graphics.Wayland.WlRoots.Input (InputDevice, getCleanDeviceName)
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

    , WlrModifier
    , getModifiers
    , getModifierPtr
    , fieldToModifiers
    , modifierInField
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

import Waymonad
    ( withSeat
    , WayLoggers (..)
    , getState
    , getSeat
    )
import Waymonad.Types
    ( Compositor (compBackend)
    , WayBindingState (wayCompositor, wayXKBMap)
    , Way (..)
    )
import Waymonad.Types.Core (WayKeyState (..), Seat(seatKeymap, seatKeyboards, seatRoots))
import Waymonad.Utility.Signal (setSignalHandler)
import Waymonad.Utility.Log (logPutStr, LogPriority (..))
import {-# SOURCE #-} Waymonad.Protocols.InputInhibit (getInhibitingClient)

import Text.XkbCommon.Context
import Text.XkbCommon.KeyboardState
import Text.XkbCommon.KeycodeList
import Text.XkbCommon.Keymap
import Text.XkbCommon.Keysym
import Text.XkbCommon.KeysymPatterns
import Text.XkbCommon.Types

import qualified Data.Set as S

data Keyboard = Keyboard
    { keyboardDevice :: Ptr WlrKeyboard
    , keyboardIDevice :: Ptr InputDevice
    } deriving (Eq, Show, Ord)

tryRunKeybind :: WayKeyState -> Way vs ws Bool
tryRunKeybind keyState = (fmap seatKeymap <$> getSeat) >>= \case
    Just bindRef -> liftIO $ do
        keybindings <- readIORef bindRef
        keybindings keyState
    Nothing -> pure False

keyStateToDirection :: KeyState -> Direction
keyStateToDirection KeyReleased = keyUp
keyStateToDirection KeyPressed  = keyDown


switchVT :: Ptr Backend -> Word -> IO ()
switchVT backend vt = do
    mSession <- getSession backend
    case mSession of
        Nothing -> pure ()
        Just s -> changeVT s vt


handleKeyPress :: Word32 -> Keysym -> Way vs a Bool
handleKeyPress modifiers sym@(Keysym key) = do
    logPutStr loggerKeybinds Trace $ "Checking for keybind: " ++ (show $ fieldToModifiers modifiers) ++ ":" ++ keysymName sym
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
        _ -> tryRunKeybind $ WayKeyState (fromIntegral modifiers) (fromIntegral key)


tellClient :: Seat -> Keyboard -> EventKey -> IO ()
tellClient seat keyboard event = do
    seatSetKeyboard   (seatRoots seat) $ keyboardIDevice keyboard
    keyboardNotifyKey (seatRoots seat) (timeSec event) (keyCode event) (state event)

handleKeySimple :: Keyboard -> CKeycode -> Way vs a Bool
handleKeySimple keyboard keycode = do
    keystate <- liftIO . getKeystate $ keyboardDevice keyboard
    keymap   <- liftIO . getKeymap $ keyboardDevice keyboard
    modifiers <- liftIO $ getModifiers $ keyboardDevice keyboard

    layoutL <- liftIO $ keyGetLayoutI keystate keycode
    syms <- liftIO $ keymapSymsByLevelI keymap keycode layoutL (CLevelIndex 0)

    handled <- forM syms $
        handleKeyPress modifiers

    pure $ or handled

isModifierPressed :: Ptr WlrKeyboard -> WlrModifier -> IO Bool
isModifierPressed ptr modi = do
    modifiers <- getModifiers ptr
    pure $ modifierInField modi modifiers

handleKeyXkb :: Keyboard -> CKeycode -> Way vs a Bool
handleKeyXkb keyboard keycode = do
    keystate <- liftIO . getKeystate $ keyboardDevice keyboard
    modifiers <- liftIO $ getModifiers $ keyboardDevice keyboard
    consumed <- liftIO $ keyGetConsumedMods2 keystate keycode

    let usedMods = modifiers .&. complement (fromIntegral consumed)

    syms <- liftIO $ getStateSymsI keystate keycode

    handled <- forM syms $
        handleKeyPress usedMods

    pure $ or handled

-- This handler is called for every key pressed on a keyboard.
-- We dispatch keybinds from this handler.
--
-- To prevent weirdness that X had with keyboard layouts, this uses an approach
-- with two checks.
-- 1) Use xkb processed modifiers and keys.
--    If [Win]+[Shift]+2 is pressed this will match:
--       [Win]+@ on us layout
--       [Win]+" on german layout
--    Though other keys are generated here, e.g. the keys used for switching
--    TTYs are bound as "Keysym_XF86Switch_VT_#" not [Alt]+[Shift]+[F#].
-- 2) If the xkb version did not match any keybinds, match without xkb
--    processing. With the same keys pressed as above this will match:
--      [Win]+[Shift]+2 on all layouts and configurations.
--    This allows to bind this for managing workspaces, and switch keyboard
--    layouts without interfering with keybinds.
-- If neither approach matches a keybind, the key-press is forwarded to the
-- focused client.
handleKeyEvent :: Keyboard -> Seat -> Ptr EventKey -> Way vs ws ()
handleKeyEvent keyboard seat ptr = withSeat (Just seat) $ do
    event <- liftIO $ peek ptr
    let keycode = fromEvdev . fromIntegral . keyCode $ event
    inhib <- isJust <$> getInhibitingClient

    handled <- if inhib
        then pure False
        else case state event of
            -- We currently don't do anything special for releases
            KeyReleased -> pure False
            KeyPressed -> do
                handled <- handleKeyXkb keyboard keycode
                if handled
                    then pure handled
                    else handleKeySimple keyboard keycode

    liftIO . unless handled $ tellClient seat keyboard event

handleModifiers :: Keyboard -> Seat -> Ptr a -> Way vs ws ()
handleModifiers keyboard seat _ = liftIO $ do
    seatSetKeyboard (seatRoots seat) $ keyboardIDevice keyboard

    keyboardNotifyModifiers (seatRoots seat) (getModifierPtr $ keyboardDevice keyboard)

handleKeyboardAdd :: Seat -> Ptr InputDevice -> Ptr WlrKeyboard -> Way vs a ()
handleKeyboardAdd seat dev ptr = do
    liftIO $ modifyIORef (seatKeyboards seat) (S.insert ptr)
    let signals = getKeySignals ptr
    rmlvoMap <- wayXKBMap <$> getState
    liftIO $ do
        name <- getCleanDeviceName dev
        (Just cxt) <- newContext defaultFlags
        (Just keymap) <- newKeymapFromNamesI cxt $ rmlvoMap name
        setKeymap ptr keymap

    let keyboard = Keyboard ptr dev

    kh <- setSignalHandler
        (keySignalKey signals)
        (handleKeyEvent keyboard seat)
    mh <- setSignalHandler
        (keySignalModifiers signals)
        (handleModifiers keyboard seat)

    liftIO $ do
        sptr <- newStablePtr (kh, mh)
        poke (getKeyDataPtr ptr) (castStablePtrToPtr sptr)


detachKeyboard :: Seat -> Ptr WlrKeyboard -> IO ()
detachKeyboard seat ptr = do
    handleKeyboardRemove seat ptr
    poke (getKeyDataPtr ptr) nullPtr

handleKeyboardRemove :: Seat -> Ptr WlrKeyboard -> IO ()
handleKeyboardRemove seat ptr = do
    dptr <- peek (getKeyDataPtr ptr)
    liftIO $ modifyIORef (seatKeyboards seat) (S.delete ptr)
    when (dptr /= nullPtr) (do
        let sptr = castPtrToStablePtr dptr
        (kh, mh) <- deRefStablePtr sptr
        removeListener kh
        removeListener mh
        freeStablePtr sptr
                           )
