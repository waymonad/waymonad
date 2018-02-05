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
module Waymonad.Input.Keyboard
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader(..), local)
import Data.Bits ((.&.), complement)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
    , fieldToModifiers
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

import Waymonad.Input.Seat
import Waymonad
    ( BindingMap
    , withSeat
    , WayBindingState (..)
    , WayLoggers (..)
    , getState
    )
import Waymonad.Types
    ( Compositor (compBackend)
    , WayBindingState (wayCompositor, wayKeybinds)
    , Way (..)
    )
import Waymonad.Utility.Signal (setSignalHandler)
import Waymonad.Utility.Log (logPutText, logPutStr, LogPriority (..))

import Text.XkbCommon.Context
import Text.XkbCommon.KeyboardState
import Text.XkbCommon.KeycodeList
import Text.XkbCommon.Keymap
import Text.XkbCommon.Keysym
import Text.XkbCommon.KeysymPatterns
import Text.XkbCommon.Types

import qualified Data.Map as M

data Keyboard vs ws = Keyboard
    { keyboardDevice :: Ptr WlrKeyboard
    , keyboardIDevice :: Ptr InputDevice
    , keyboardBindings :: IORef (BindingMap vs ws)
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
        _ -> case M.lookup (modifiers, key) bindings of
                Nothing -> pure False
                Just fun -> do
                    logPutText loggerKeybinds Debug "Found a keybind"
                    fun
                    pure True


tellClient :: Seat -> Keyboard vs ws -> EventKey -> IO ()
tellClient seat keyboard event = do
    seatSetKeyboard   (seatRoots seat) $ keyboardIDevice keyboard
    keyboardNotifyKey (seatRoots seat) (timeSec event) (keyCode event) (state event)

handleKeySimple
    :: BindingMap vs a
    -> Keyboard vs a
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
    -> Keyboard vs a
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
handleKeyEvent :: Keyboard vs ws
               -> Seat
               -> Ptr EventKey
               -> Way vs ws ()
handleKeyEvent keyboard seat ptr = withSeat (Just seat) $ do
    bindings <- liftIO . readIORef $ keyboardBindings keyboard
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

handleModifiers :: Keyboard vs ws -> Seat -> Ptr a -> Way vs ws ()
handleModifiers keyboard seat _ = liftIO $ do
    seatSetKeyboard (seatRoots seat) $ keyboardIDevice keyboard

    keyboardNotifyModifiers (seatRoots seat) (getModifierPtr $ keyboardDevice keyboard)

getSubMap :: Way vs ws (BindingMap vs ws)
getSubMap =  do
    ref <- wayCurrentKeybinds <$> getState
    liftIO $ readIORef ref

setSubMap :: BindingMap vs ws -> Way vs ws ()
setSubMap subMap = do
    let keys = M.keys subMap
        klines = map (\(modifiers, sym) -> (show $ fieldToModifiers modifiers) ++ ":" ++ keysymName (Keysym sym)) keys
        logLine = unlines klines

    logPutStr loggerKeybinds Trace $ "Loading submap with keybinds:\n" ++ logLine
    ref <- wayCurrentKeybinds <$> getState
    liftIO $ writeIORef ref subMap

resetSubMap :: Way vs ws ()
resetSubMap = setSubMap =<< (wayKeybinds <$> getState)

withBindingRef :: IORef (BindingMap vs ws) -> Way vs ws a -> Way vs ws a
withBindingRef ref (Way act) = Way $ local (\s -> s {wayCurrentKeybinds = ref}) act

handleKeyboardAdd
    :: Seat
    -> Ptr InputDevice
    -> Ptr WlrKeyboard
    -> Way vs a ()
handleKeyboardAdd seat dev ptr = do
    let signals = getKeySignals ptr
    bindings <- wayKeybinds <$> getState

    let keys = M.keys bindings
        klines = map (\(modifiers, sym) -> (show $ fieldToModifiers modifiers) ++ ":" ++ keysymName (Keysym sym)) keys
        logLine = unlines klines

    logPutStr loggerKeybinds Trace $ "Loading keyboard with keybinds:\n" ++ logLine

    liftIO $ do
        (Just cxt) <- newContext defaultFlags
        (Just keymap) <- newKeymapFromNamesI cxt noPrefs
        setKeymap ptr keymap

    mapRef <- liftIO $ newIORef bindings

    withBindingRef mapRef $ do
        let keyboard = Keyboard ptr dev mapRef

        kh <- setSignalHandler
            (keySignalKey signals)
            (handleKeyEvent keyboard seat)
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
