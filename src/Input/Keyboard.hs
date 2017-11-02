module Input.Keyboard
where

import Data.Word (Word32)
import Data.IORef (IORef)
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
import Graphics.Wayland.WlRoots.Seat (WlrSeat, keyboardNotifyKey, keyboardNotifyModifiers, seatSetKeyboard)
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

    , KeyboardModifiers (..)
    , readModifiers

    , getModifiers
    )
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )
import Control.Monad (forM, when)

import Waymonad (WayStateRef, LayoutCacheRef, BindingMap, WayBindingState (..), runWayBinding)

import Text.XkbCommon.Keymap
import Text.XkbCommon.Types
import Text.XkbCommon.Context
import Text.XkbCommon.KeyboardState
import Text.XkbCommon.KeycodeList
import Text.XkbCommon.KeysymPatterns

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
    :: Ord a
    => DisplayServer
    -> Ptr Backend
    -> Ptr WlrSeat
    -> Word32
    -> Keysym
    -> LayoutCacheRef
    -> IORef Int
    -> IORef [(a, Int)]
    -> WayStateRef a
    -> BindingMap a
    -> IO Bool
handleKeyPress dsp backend seat modifiers sym@(Keysym key) cacheRef currentOut wsMapping stateRef bindings = case sym of
        Keysym_Escape -> displayTerminate dsp >> pure True
        -- Would be cooler if this wasn't a listing of VTs (probably TH)
        Keysym_XF86Switch_VT_1  -> switchVT backend 1  >> pure True
        Keysym_XF86Switch_VT_2  -> switchVT backend 2  >> pure True
        Keysym_XF86Switch_VT_3  -> switchVT backend 3  >> pure True
        Keysym_XF86Switch_VT_4  -> switchVT backend 4  >> pure True
        Keysym_XF86Switch_VT_5  -> switchVT backend 5  >> pure True
        Keysym_XF86Switch_VT_6  -> switchVT backend 6  >> pure True
        Keysym_XF86Switch_VT_7  -> switchVT backend 7  >> pure True
        Keysym_XF86Switch_VT_8  -> switchVT backend 8  >> pure True
        Keysym_XF86Switch_VT_9  -> switchVT backend 9  >> pure True
        Keysym_XF86Switch_VT_10 -> switchVT backend 10 >> pure True
        Keysym_XF86Switch_VT_11 -> switchVT backend 11 >> pure True
        Keysym_XF86Switch_VT_12 -> switchVT backend 12 >> pure True
        _ -> case M.lookup (modifiers, key) bindings of
                Nothing -> pure False
                Just fun -> let state = WayBindingState cacheRef stateRef currentOut wsMapping seat
                             in runWayBinding state fun >> pure True



tellClient :: Ptr WlrSeat -> Keyboard -> EventKey -> IO ()
tellClient seat keyboard event = do
    seatSetKeyboard seat $ keyboardIDevice keyboard
    keyboardNotifyKey seat (timeSec event) (keyCode event) (state event)

handleKeyEvent
    :: Ord a
    => DisplayServer
    -> Ptr Backend
    -> Keyboard
    -> Ptr WlrSeat
    -> LayoutCacheRef
    -> IORef Int
    -> IORef [(a, Int)]
    -> WayStateRef a
    -> BindingMap a
    -> Ptr EventKey
    -> IO ()
handleKeyEvent dsp backend keyboard seat cacheRef currentOut wsMapping stateRef bindings ptr = do
    event <- peek ptr
    let keycode = fromEvdev . fromIntegral . keyCode $ event
    keyState <- getKeystate $ keyboardDevice keyboard
    syms <- getStateSymsI keyState keycode
    modifiers <- getModifiers $ keyboardDevice keyboard
    handled <- forM syms $ \sym -> case (state event) of
        -- We currently don't do anything special for releases
        KeyReleased -> pure False
        KeyPressed ->
            handleKeyPress dsp backend seat modifiers sym cacheRef currentOut wsMapping stateRef bindings

    when (not $ foldr (||) False handled) $ tellClient seat keyboard event

handleModifiers :: Keyboard -> Ptr WlrSeat -> Ptr a -> IO ()
handleModifiers keyboard seat _ = do
    mods <- readModifiers $ keyboardDevice keyboard
    seatSetKeyboard seat $ keyboardIDevice keyboard
    keyboardNotifyModifiers seat (modDepressed mods) (modLatched mods) (modLocked mods) (modGroup mods)

handleKeyboardAdd
    :: Ord a
    => DisplayServer
    -> Ptr Backend
    -> Ptr WlrSeat
    -> LayoutCacheRef
    -> IORef Int
    -> IORef [(a, Int)]
    -> WayStateRef a
    -> BindingMap a
    -> Ptr InputDevice
    -> Ptr WlrKeyboard
    -> IO ()
handleKeyboardAdd dsp backend seat cacheRef currentOut wsMapping stateRef bindings dev ptr = do
    let signals = getKeySignals ptr

    (Just cxt) <- newContext defaultFlags
    (Just keymap) <- newKeymapFromNamesI cxt noPrefs

    setKeymap ptr keymap

    let keyboard = Keyboard ptr dev

    kh <- addListener (WlListener $ handleKeyEvent dsp backend keyboard seat cacheRef currentOut wsMapping stateRef bindings) (keySignalKey signals)
    mh <- addListener (WlListener $ handleModifiers keyboard seat) (keySignalModifiers signals)
    sptr <- newStablePtr (kh, mh)
    poke (getKeyDataPtr ptr) (castStablePtrToPtr sptr)


handleKeyboardRemove :: Ptr WlrKeyboard -> IO ()
handleKeyboardRemove ptr = do
    sptr <- peek (getKeyDataPtr ptr)
    freeStablePtr $ castPtrToStablePtr sptr
