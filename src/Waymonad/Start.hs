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
module Waymonad.Start
    ( launchCompositor
    , CompHooks (..)
    , ignoreHooks
    , FrameHandler
    , Bracketed (..)
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import UnliftIO.Exception (bracket)
import System.Clock (toNanoSecs , getTime , Clock(Monotonic))
import Foreign.Storable (Storable(peek, poke))
import Foreign.Ptr (Ptr)
import Data.Maybe (listToMaybe)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    , deRefStablePtr
    )
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import Graphics.Wayland.WlRoots.Backend
    ( Backend, backendAutocreate, backendStart
    , BackendSignals(..), backendGetSignals
    )
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

import Foreign.C.String (withCString)
import System.IO (hPutStr, hPutStrLn, stderr, hPrint)
import System.Environment (lookupEnv, getEnv, setEnv, unsetEnv)
import Text.XkbCommon.Types

import Foreign.C.Types (CChar)
import Foreign.C.String
import Waymonad.ViewSet (FocusCore, WSTag)
import Waymonad (getState)
import Waymonad.Types (Way, WayBindingState (wayCoreShells))
import Waymonad.Shells (startShell, stopShell)


import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , removeListener
    )

data Bracketed vs a b
    = forall c. Bracketed
        { bracketSetup    :: a -> Way vs b c
        , bracketTeardown :: c -> Way vs b ()
        }
  | PreBracket (forall c. a -> Way vs b c -> Way vs b c)

runBracket :: Bracketed vs a c -> a -> (a -> Way vs c b) -> Way vs c b
runBracket Bracketed {bracketSetup = setup, bracketTeardown = teardown} val act =
    bracket (setup val) teardown (const $ act val)
runBracket (PreBracket fun) val act = fun val (act val)

foldBrackets :: [Bracketed vs a c] -> (a -> Way vs c b) -> a -> Way vs c b
foldBrackets [] act val = act val
foldBrackets (b:bs) act val = runBracket b val (foldBrackets bs act)

type FrameHandler = Double -> Ptr WlrOutput -> IO ()

data CompHooks vs a = CompHooks
    { displayHook :: [Bracketed vs DisplayServer a]
    , backendPreHook :: [Bracketed vs (DisplayServer, Ptr Backend) a]
    , backendPostHook :: [Bracketed vs () a]

    , outputAddHook :: Ptr WlrOutput -> IO FrameHandler
    , keyPressHook :: Keysym -> Direction -> IO ()
    , outputRemoveHook :: Ptr WlrOutput -> IO ()
    }


ignoreHooks :: CompHooks vs a
ignoreHooks = CompHooks
    { displayHook = []
    , backendPreHook = []
    , backendPostHook = []
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

handleOutputAdd :: CompHooks vs a -> Ptr WlrOutput -> IO ()
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

handleOutputRemove :: CompHooks vs a -> Ptr WlrOutput -> IO ()
handleOutputRemove hooks output = do
    ptr :: Ptr () <- peek (getDataPtr output)
    let sptr =  castPtrToStablePtr ptr
    removeListener =<< deRefStablePtr sptr
    freeStablePtr sptr
    outputRemoveHook hooks output

foreign import ccall "wl_display_add_socket_auto" c_add_socket_auto :: Ptr DisplayServer -> IO (Ptr CChar)

foreign import ccall "wl_display_add_socket" c_add_socket :: Ptr DisplayServer -> Ptr CChar -> IO (Ptr CChar)

backendMain :: (FocusCore vs a, WSTag a) => CompHooks vs a -> DisplayServer -> Ptr Backend -> Way vs a ()
backendMain hooks display backend = do
    shells <- wayCoreShells <$> getState
    mapM_ startShell shells
    -- This dispatches the first events, e.g. output/input add signals
    liftIO $ do
        backendStart backend
        setEnv "WAYLAND_DISPLAY" =<< getEnv "_WAYLAND_DISPLAY"
        unsetEnv "_WAYLAND_DISPLAY"
    -- Start the hooks that want to run *after* the backend got initialised and
    -- run the display
    foldBrackets (backendPostHook hooks) (const $ liftIO $ displayRun display) ()
    mapM_ stopShell shells

bindSocket :: MonadIO m => DisplayServer -> m ()
bindSocket display = liftIO $ do
    waySocket <- lookupEnv "WAYMONAD_DISPLAY"
    let addFun dsp = case waySocket of
            Nothing -> peekCString =<< c_add_socket_auto dsp
            Just name -> do
                void $ withCString name $ c_add_socket dsp
                unsetEnv "WAYMONAD_DISPLAY"
                pure name
    sName <- (\(DisplayServer ptr) -> addFun ptr) display
    hPutStr stderr "Opened on socket: "
    hPutStrLn stderr sName
    setEnv "_WAYLAND_DISPLAY" sName

displayMain :: (FocusCore vs a, WSTag a) => CompHooks vs a -> DisplayServer -> Way vs a ()
displayMain hooks display = do
    let outAdd = Bracketed (liftIO . addListener (WlListener $ handleOutputAdd hooks) . outputAdd . backendGetSignals . snd) (liftIO .  removeListener)
    let outRem = Bracketed (liftIO . addListener (WlListener $ handleOutputRemove hooks) . outputRemove . backendGetSignals . snd) (liftIO .  removeListener)
    liftIO $ do
        dsp <- lookupEnv "WAYLAND_DISPLAY"
        -- Prevent the idiotic defaulting behaviour of libwayland
        case dsp of
            Nothing -> setEnv "WAYLAND_DISPLAY" "What are you even doing?"
            Just _ -> pure ()
    foldBrackets (outAdd: outRem: backendPreHook hooks) (uncurry $ backendMain hooks) . (display, ) =<< (liftIO $ backendAutocreate display)

launchCompositor :: (FocusCore vs a, WSTag a) => CompHooks vs a -> Way vs a ()
launchCompositor hooks = 
    let binder = Bracketed (bindSocket) (const $ pure ())
     in foldBrackets (binder: displayHook hooks) (displayMain hooks) =<< liftIO displayCreate
