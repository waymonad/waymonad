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
module Waymonad.Systemd (getSystemdBracket)
where

#include <sys/socket.h>

import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (for)
import Data.Word (Word64)
import Foreign.C.Error (Errno(..), errnoToIOError)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import System.Posix.Types (Fd(..))

import Graphics.Wayland.Server (DisplayServer (..))

import Waymonad.Start (Bracketed (..))
import Waymonad.Utility.Base (doJust, whenJust)


foreign import ccall unsafe "sd_listen_fds" listen_fds :: CInt -> IO CInt
foreign import ccall unsafe "sd_is_socket" is_socket :: Fd -> CInt -> CInt -> CInt -> IO CInt

getListenFds :: IO [Fd]
getListenFds = do
    count <- listen_fds 1
    if count < 0 
        then fail "Failed to get the activation fds from systemd"
        else for (map Fd . take (fromIntegral count) $ [3..]) $ \fd -> do
            ret <- is_socket fd #{const AF_UNIX} #{const SOCK_STREAM} 1
            case compare ret 0 of
                GT -> pure fd
                EQ -> fail "Systemd provided socket is of wrong type/not listening"
                LT -> fail "Failed verify systemd provided fd is socket"

foreign import ccall unsafe "wl_display_add_socket_fd" add_fd :: Ptr DisplayServer -> Fd -> IO CInt

addListenFds :: DisplayServer -> [Fd] -> IO ()
addListenFds (DisplayServer dsp) = mapM_  (add_fd dsp)

manageSystemdFds :: DisplayServer -> IO ()
manageSystemdFds dsp = addListenFds dsp =<< getListenFds

foreign import ccall unsafe "sd_watchdog_enabled" watchdog_enabled :: CInt -> Ptr Word64 -> IO CInt

getWatchdogTime :: IO (Maybe Word64)
getWatchdogTime = alloca $ \ptr -> do
    ret <- watchdog_enabled 1 ptr
    case compare ret 0 of
        GT -> Just <$> peek ptr
        EQ -> pure Nothing
        LT -> ioError $ errnoToIOError "getWatchdogTime" (Errno ret) Nothing Nothing

foreign import ccall unsafe "sd_notify" notify :: CInt -> CString -> IO CInt

watchdogLoop :: Int -> CString -> IO ()
watchdogLoop time str = forever $ do
    _ <- notify 0 str
    threadDelay time

forkWatchdogLoop :: IO (Maybe ThreadId)
forkWatchdogLoop = doJust getWatchdogTime $ \time -> do
    let actual = time * 3 `div` 4
    fmap Just . forkIO $ withCString "WATCHDOG=1" $ watchdogLoop (fromIntegral actual)

doSystemdStartup :: DisplayServer -> IO (Maybe ThreadId)
doSystemdStartup dsp = do
    manageSystemdFds dsp
    _ <- withCString "READY=1" $ notify 0
    forkWatchdogLoop

stopSystemd :: Maybe ThreadId -> IO ()
stopSystemd thread = do
    _ <- withCString  "STOPPING=1" $ notify 1
    whenJust thread killThread

getSystemdBracket :: Bracketed vs DisplayServer ws
getSystemdBracket = Bracketed (liftIO . doSystemdStartup) (liftIO . stopSystemd)
