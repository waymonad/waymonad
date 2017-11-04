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
import Graphics.Wayland.WlRoots.Backend
    ( Backend, backendAutocreate, backendStart
    , BackendSignals(..), backendGetSignals
    )
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Output
    ( Output
    , getName
    , getModes
    , setOutputMode

    , OutputSignals(..)
    , getOutputSignals
    , getDataPtr
    )
import Graphics.Wayland.Server
    ( DisplayServer (..)
    , displayCreate
    , displayRun
    )

import System.IO (hPutStr, hPutStrLn, stderr)
import Text.XkbCommon.Types

import Foreign.C.Types (CChar)
import Foreign.C.String
import System.Environment (setEnv)


import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    , removeListener
    )

data Handlers = Handlers ListenerToken ListenerToken ListenerToken ListenerToken

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

handleOutputAdd :: CompHooks -> Ptr Output -> IO ()
handleOutputAdd hooks output = do
    hPutStr stderr "Found output: "
    hPutStrLn stderr =<< getName output

    modes <- getModes output
    readable <- mapM peek modes
    hPutStrLn stderr $ show readable

    case listToMaybe $ reverse modes of
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
addSignalHandlers hooks _ ptr =
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
