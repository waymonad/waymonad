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
    , injectBracket

    , Inject (..)
    )
where

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Utils (with)
import System.Posix.Types (Fd)
import System.Posix.IO (createPipe, fdRead, fdWriteBuf, setFdOption, FdOption(CloseOnExec))
import Graphics.Wayland.Server (displayGetEventLoop, eventLoopAddFd, clientStateReadable, DisplayServer)
import Data.Typeable (Typeable)

import Shared
import Waymonad (makeCallback2)
import WayUtil
import Waymonad.Types (Way)
import Waymonad.Extensible (ExtensionClass (..))

data Inject vs ws = Inject (Way vs ws ())

data InjectChan vs ws = InjectChan
    { injectChan  :: TChan (Inject vs ws)
    , injectWrite :: Fd
    , injectRead  :: Fd
    }

instance (Typeable vs, Typeable ws) => ExtensionClass (InjectChan vs ws) where
    initialValue = unsafePerformIO makeInject

handleInjected :: Inject vs ws -> Way vs ws ()
handleInjected (Inject act) = act

readInjectEvt :: InjectChan vs ws -> Way vs ws ()
readInjectEvt chan = do
    next <- liftIO . atomically . tryReadTChan $ injectChan chan
    case next of
        Just x -> do
            void . liftIO $ fdRead (injectRead chan) 1
            handleInjected x
            readInjectEvt chan
        Nothing -> pure ()

injectEvt :: (Typeable vs, Typeable ws) => Inject vs ws -> Way vs ws ()
injectEvt inj = do
    chan <- getEState
    liftIO . atomically $ writeTChan (injectChan chan) inj
    void . liftIO $ with 1 $ \ptr -> fdWriteBuf (injectWrite chan) ptr 1

registerInjectHandler :: (Typeable vs, Typeable ws) => DisplayServer -> Way vs ws ()
registerInjectHandler display = do
    chan <- getEState
    evtLoop <- liftIO $ displayGetEventLoop display
    cb <- makeCallback2 $ \_ _ -> readInjectEvt chan >> pure False

    void . liftIO $ eventLoopAddFd
        evtLoop
        (injectRead chan)
        clientStateReadable
        cb

makeInject :: IO (InjectChan vs ws)
makeInject = do
    (readFd, writeFd) <- liftIO createPipe
    liftIO $ setFdOption readFd CloseOnExec True
    liftIO $ setFdOption writeFd CloseOnExec True
    chan <- newTChanIO
    pure $ InjectChan chan writeFd readFd

injectBracket :: (Typeable vs, Typeable ws) => Bracketed vs DisplayServer ws
injectBracket = Bracketed (\dsp -> do
    -- setEState =<< liftIO makeInject
    registerInjectHandler dsp
                          ) (const $ pure ())
