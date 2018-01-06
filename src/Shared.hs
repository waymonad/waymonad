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
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
module Shared
    ( launchCompositor
    , CompHooks (..)
    , ignoreHooks
    , FrameHandler
    , Bracketed (..)
    )
where

import Control.Exception (bracket)
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
    ( WlrOutput
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

import System.IO (hPutStr, hPutStrLn, stderr, hPrint)
import Text.XkbCommon.Types

import Foreign.C.Types (CChar)
import Foreign.C.String
import System.Environment (setEnv)


import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , removeListener
    )

data Bracketed a
    = forall b. Bracketed
        { bracketSetup    :: a -> IO b
        , bracketTeardown :: b -> IO ()
        }
  | PreBracket (forall b. a -> IO b -> IO b)

runBracket :: Bracketed a -> a -> (a -> IO b) -> IO b
runBracket Bracketed {bracketSetup = setup, bracketTeardown = teardown} val act =
    bracket (setup val) teardown (const $ act val)
runBracket (PreBracket fun) val act = fun val (act val)

foldBrackets :: [Bracketed a] -> (a -> IO b) -> a -> IO b
foldBrackets [] act val = act val
foldBrackets (b:bs) act val = runBracket b val (foldBrackets bs act)

type FrameHandler = Double -> Ptr WlrOutput -> IO ()

data CompHooks = CompHooks
    { displayHook :: [Bracketed DisplayServer]
    , backendPreHook :: [Bracketed (DisplayServer, Ptr Backend)]
    , backendPostHook :: [Bracketed ()]

    , inputAddHook :: Ptr InputDevice -> IO ()
    , outputAddHook :: Ptr WlrOutput -> IO FrameHandler
    , keyPressHook :: Keysym -> Direction -> IO ()
    , outputRemoveHook :: Ptr WlrOutput -> IO ()
    }


ignoreHooks :: CompHooks
ignoreHooks = CompHooks
    { displayHook = []
    , backendPreHook = []
    , backendPostHook = []
    , inputAddHook = \_ -> pure ()
    , outputAddHook = \_ -> pure $ \_ _ -> pure ()
    , keyPressHook = \_ _ -> pure ()
    , outputRemoveHook = \_ -> pure ()
    }


handleFrame :: FrameHandler -> IORef Integer -> Ptr WlrOutput -> IO ()
handleFrame hook ref output = do
    old <- readIORef ref
    time <- toNanoSecs <$> getTime Monotonic
    writeIORef ref time

    let timeDiff = time - old
    let secs :: Double = fromIntegral timeDiff / 1e9

    hook secs output

handleOutputAdd :: CompHooks -> Ptr WlrOutput -> IO ()
handleOutputAdd hooks output = do
    modes <- getModes output
    readable <- mapM peek modes
    hPrint stderr readable

    case listToMaybe $ reverse modes of
        Nothing -> pure ()
        Just x -> setOutputMode x output

    ref <- newIORef 0
    let signals = getOutputSignals output
    frame <- outputAddHook hooks output
    handler <- addListener (WlListener (\_ -> handleFrame frame ref output)) (outSignalFrame signals)

    sptr <- newStablePtr handler
    poke (getDataPtr output) (castStablePtrToPtr sptr)

handleOutputRemove :: CompHooks -> Ptr WlrOutput -> IO ()
handleOutputRemove hooks output = do
    sptr :: Ptr () <- peek (getDataPtr output)
    freeStablePtr $ castPtrToStablePtr sptr
    outputRemoveHook hooks output

foreign import ccall "wl_display_add_socket_auto" c_add_socket :: Ptr DisplayServer -> IO (Ptr CChar)


backendMain :: CompHooks -> DisplayServer -> Ptr Backend -> IO ()
backendMain hooks display backend = do
    -- This dispatches the first events, e.g. output/input add signals
    backendStart backend

    -- Start the hooks that want to run *after* the backend got initialised and
    -- run the display
    foldBrackets (backendPostHook hooks) (const $ displayRun display) ()

bindSocket :: DisplayServer -> IO ()
bindSocket display = do
    socket <- (\(DisplayServer ptr) -> c_add_socket ptr) display
    sName <- peekCString socket
    hPutStr stderr "Opened on socket: "
    hPutStrLn stderr sName
    setEnv "_WAYLAND_DISPLAY" sName

displayMain :: CompHooks -> DisplayServer -> IO ()
displayMain hooks display = do
    let binder = Bracketed (const $ bindSocket display) (const $ pure ())
    let outAdd = Bracketed (addListener (WlListener $ handleOutputAdd hooks) . outputAdd . backendGetSignals . snd) removeListener
    let outRem = Bracketed (addListener (WlListener $ handleOutputRemove hooks) . outputRemove . backendGetSignals . snd) removeListener
    foldBrackets (binder: outAdd: outRem: backendPreHook hooks) (uncurry $ backendMain hooks) . (display, ) =<< backendAutocreate display

launchCompositor :: CompHooks -> IO ()
launchCompositor hooks = foldBrackets (displayHook hooks) (displayMain hooks) =<< displayCreate
