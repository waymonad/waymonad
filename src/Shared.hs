{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Shared
    ( launchCompositor
    , CompHooks (..)
    , ignoreHooks
    , FrameHandler
    )
where

import System.Clock
    ( toNanoSecs
    , getTime
    , Clock(Monotonic)
    )
import Foreign.Storable (Storable(peek, poke))
import Foreign.Ptr (Ptr)
import Data.Maybe (listToMaybe)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import Graphics.Wayland.WlRoots.Backend.Multi (getSession')
import Graphics.Wayland.WlRoots.Backend.Session (changeVT)
import Graphics.Wayland.WlRoots.Backend
    ( Backend, backendAutocreate, backendStart
    , BackendSignals(..), backendGetSignals
    )
import Graphics.Wayland.WlRoots.Input
    ( InputDevice
    , inputDeviceType
    , DeviceType(..)
    )
import Graphics.Wayland.WlRoots.Output
    ( Output
    , getName
    , getModes
    , setOutputMode

    , OutputSignals(..)
    , getOutputSignals
    , getDataPtr
    )
import Graphics.Wayland.WlRoots.Input.Keyboard
    ( WlrKeyboard
    , KeyboardSignals (..)
    , getKeySignals
    , getKeyDataPtr
    , EventKey (..)
    , KeyState (..)
    , setKeymap
    , getKeystate
    )
import Graphics.Wayland.Server
    ( DisplayServer (..)
    , displayCreate
    , displayRun
    , displayTerminate
    )

import Data.List (intercalate)
import System.IO (hPutStr, hPutStrLn, stderr)
import Text.XkbCommon.Keymap
import Text.XkbCommon.Keysym
import Text.XkbCommon.Types
import Text.XkbCommon.Context
import Text.XkbCommon.KeyboardState
import Text.XkbCommon.KeycodeList
import Text.XkbCommon.KeysymPatterns

import Foreign.C.Types (CChar)
import Foreign.C.String
import System.Environment (setEnv)

import Control.Monad (forM_)

import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    , removeListener
    )

data Handlers = Handlers ListenerToken ListenerToken ListenerToken ListenerToken

keyStateToDirection :: KeyState -> Direction
keyStateToDirection KeyReleased = keyUp
keyStateToDirection KeyPressed  = keyDown

type FrameHandler = Double -> Ptr Output -> IO ()

data CompHooks = CompHooks
    { displayHook :: DisplayServer -> IO ()
    , backendPreHook :: Ptr Backend -> IO ()
    , backendPostHook :: Ptr Backend -> IO ()

    , inputAddHook :: Ptr InputDevice -> IO ()
    , outputAddHook :: Ptr Output -> IO FrameHandler
    , keyPressHook :: Keysym -> Direction -> IO ()
    , outputRemoveHook :: Ptr Output -> IO ()
    }


ignoreHooks :: CompHooks
ignoreHooks = CompHooks
    { displayHook = \_ -> pure ()
    , backendPreHook = \_ -> pure ()
    , backendPostHook = \_ -> pure ()
    , inputAddHook = \_ -> pure ()
    , outputAddHook = \_ -> pure $ \_ _ -> pure ()
    , keyPressHook = \_ _ -> pure ()
    , outputRemoveHook = \_ -> pure ()
    }


handleFrame :: FrameHandler -> IORef Integer -> Ptr Output -> IO ()
handleFrame hook ref output = do
    old <- readIORef ref
    time <- toNanoSecs <$> getTime Monotonic
    writeIORef ref time

    let timeDiff = time - old
    let secs :: Double = fromIntegral timeDiff / 1e9

    hook secs output

switchVT :: Ptr Backend -> Word -> IO ()
switchVT backend vt = do
    mSession <- getSession' backend
    case mSession of
        Nothing -> pure ()
        Just s -> changeVT s vt

handleKeyPress :: CompHooks -> DisplayServer -> Ptr Backend -> Ptr WlrKeyboard -> Ptr EventKey -> IO ()
handleKeyPress hooks dsp backend keyboard ptr = do
    event <- peek ptr
    let keycode = fromEvdev . fromIntegral . keyCode $ event
    keyState <- getKeystate keyboard
    syms <- getStateSymsI keyState keycode
    let keyDir = (keyStateToDirection $ state event)
    forM_ syms $ \sym -> case sym of
        Keysym_Escape -> displayTerminate dsp
        -- Would be cooler if this wasn't a listing of VTs (probably TH)
        Keysym_XF86Switch_VT_1  -> switchVT backend 1
        Keysym_XF86Switch_VT_2  -> switchVT backend 2
        Keysym_XF86Switch_VT_3  -> switchVT backend 3
        Keysym_XF86Switch_VT_4  -> switchVT backend 4
        Keysym_XF86Switch_VT_5  -> switchVT backend 5
        Keysym_XF86Switch_VT_6  -> switchVT backend 6
        Keysym_XF86Switch_VT_7  -> switchVT backend 7
        Keysym_XF86Switch_VT_8  -> switchVT backend 8
        Keysym_XF86Switch_VT_9  -> switchVT backend 9
        Keysym_XF86Switch_VT_10 -> switchVT backend 10
        Keysym_XF86Switch_VT_11 -> switchVT backend 11
        Keysym_XF86Switch_VT_12 -> switchVT backend 12
        _ -> keyPressHook hooks sym keyDir

handleKeyboardAdd :: CompHooks -> DisplayServer -> Ptr Backend -> Ptr WlrKeyboard -> IO ()
handleKeyboardAdd hooks dsp backend ptr = do
    let signals = getKeySignals ptr

    (Just cxt) <- newContext defaultFlags
    (Just keymap) <- newKeymapFromNamesI cxt noPrefs

    setKeymap ptr keymap

    handler <- addListener (WlListener $ handleKeyPress hooks dsp backend ptr) (keySignalKey signals)
    sptr <- newStablePtr handler
    poke (getKeyDataPtr ptr) (castStablePtrToPtr sptr)

handleInputAdd :: CompHooks -> DisplayServer -> Ptr Backend -> Ptr InputDevice -> IO ()
handleInputAdd hooks dsp backend ptr = do
    putStr "Found a new input of type: "
    iType <- inputDeviceType ptr
    print iType
    case iType of
        (DeviceKeyboard kptr) -> handleKeyboardAdd hooks dsp backend kptr
        _ -> pure ()
    inputAddHook hooks ptr

handleKeyboardRemove :: Ptr WlrKeyboard -> IO ()
handleKeyboardRemove ptr = do
    sptr <- peek (getKeyDataPtr ptr)
    freeStablePtr $ castPtrToStablePtr sptr

handleInputRemove :: CompHooks -> Ptr InputDevice -> IO ()
handleInputRemove _ ptr = do
    iType <- inputDeviceType ptr
    case iType of
        (DeviceKeyboard kptr) -> handleKeyboardRemove kptr
        _ -> pure ()

handleOutputAdd :: CompHooks -> Ptr Output -> IO ()
handleOutputAdd hooks output = do
    hPutStr stderr "Found output: "
    hPutStrLn stderr =<< getName output

    modes <- getModes output
    case listToMaybe modes of
        Nothing -> pure ()
        Just x -> setOutputMode x output

    ref <- newIORef 0
    let signals = getOutputSignals output
    frame <- outputAddHook hooks output
    handler <- addListener (WlListener (\_ -> handleFrame frame ref output)) (outSignalFrame signals)

    sptr <- newStablePtr handler
    poke (getDataPtr output) (castStablePtrToPtr sptr)

handleOutputRemove :: CompHooks -> Ptr Output -> IO ()
handleOutputRemove hooks output = do
    sptr :: Ptr () <- peek (getDataPtr output)
    freeStablePtr $ castPtrToStablePtr sptr
    outputRemoveHook hooks output


addSignalHandlers :: CompHooks -> DisplayServer -> Ptr Backend -> IO Handlers
addSignalHandlers hooks dsp ptr =
    let signals = backendGetSignals ptr
     in Handlers
        <$> addListener (WlListener $ const $ pure ()) (inputAdd signals)
        <*> addListener (WlListener $ const $ pure ()) (inputRemove signals)
        <*> addListener (WlListener $ handleOutputAdd hooks) (outputAdd signals)
        <*> addListener (WlListener $ handleOutputRemove hooks ) (outputRemove signals)

foreign import ccall "wl_display_add_socket_auto" c_add_socket :: Ptr DisplayServer -> IO (Ptr CChar)

launchCompositor :: CompHooks -> IO ()
launchCompositor hooks = do
    display <- displayCreate
    displayHook hooks display

    backend <- backendAutocreate display
    handlers <- addSignalHandlers hooks display backend

    socket <- (\(DisplayServer ptr) -> c_add_socket ptr) display
    sName <- peekCString socket
    hPutStr stderr "Opened on socket: "
    hPutStrLn stderr sName
    setEnv "_WAYLAND_DISPLAY" sName

    backendPreHook hooks backend
    backendStart backend
    backendPostHook hooks backend

    displayRun display

    let Handlers h1 h2 h3 h4 = handlers
    removeListener h1
    removeListener h2
    removeListener h3
    removeListener h4
