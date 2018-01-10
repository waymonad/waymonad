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
module InjectRunner
    ( InjectChan
    , makeInject
    , registerInjectHandler
    , injectEvt

    , Inject (..)
    )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import System.Posix.Types (Fd)
import System.Posix.IO (createPipe, fdRead, fdWriteBuf)
import Graphics.Wayland.Server (displayGetEventLoop, eventLoopAddFd, clientStateReadable, DisplayServer)

import Graphics.Wayland.WlRoots.Box (Point (..))
import Graphics.Wayland.WlRoots.Output (OutputMode, setOutputMode, setOutputScale)
import Graphics.Wayland.WlRoots.OutputLayout (moveOutput)

import Output (Output (outputRoots))
import Waymonad (getState, makeCallback2)
import Waymonad.Types (Way, WayBindingState (..), Compositor (..))

data Inject
    = ChangeMode Output (Ptr OutputMode)
    | ChangeScale Output Float
    | ChangePosition Output Point

data InjectChan = InjectChan
    { injectChan  :: TChan Inject
    , injectWrite :: Fd
    , injectRead  :: Fd
    }

handleInjected :: Inject -> Way vs a ()
handleInjected (ChangeMode out mode) =
    liftIO $ setOutputMode mode (outputRoots out)
handleInjected (ChangeScale out scale) =
    liftIO $ setOutputScale (outputRoots out) scale
handleInjected (ChangePosition out (Point x y)) = do
    layout <- compLayout . wayCompositor <$> getState
    liftIO $ moveOutput layout (outputRoots out) x y

readInjectEvt :: InjectChan -> Way vs a ()
readInjectEvt chan = do
    next <- liftIO . atomically . tryReadTChan $ injectChan chan
    case next of
        Just x -> do
            void . liftIO $ fdRead (injectRead chan) 1
            handleInjected x
            readInjectEvt chan
        Nothing -> pure ()

injectEvt :: Inject -> Way vs a ()
injectEvt inj = do
    chan <- wayInjectChan <$> getState
    liftIO . atomically $ writeTChan (injectChan chan) inj
    void . liftIO $ with 1 $ \ptr -> fdWriteBuf (injectWrite chan) ptr 1

registerInjectHandler :: DisplayServer -> Way vs a ()
registerInjectHandler display = do
    chan <- wayInjectChan <$> getState
    evtLoop <- liftIO $ displayGetEventLoop display
    cb <- makeCallback2 $ \_ _ -> readInjectEvt chan >> pure False

    void . liftIO $ eventLoopAddFd
        evtLoop
        (injectRead chan)
        clientStateReadable
        cb

makeInject :: IO InjectChan
makeInject = do
    (readFd, writeFd) <- liftIO createPipe
    chan <- newTChanIO
    pure $ InjectChan chan writeFd readFd
