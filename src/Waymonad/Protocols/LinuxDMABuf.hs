module Waymonad.Protocols.LinuxDMABuf
where

import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.LinuxDMABuf (createDMABuf, destroyDMABuf)
import Graphics.Wayland.WlRoots.Backend (Backend)

import Waymonad.Start (Bracketed (..))

getLinuxDMABufBracket :: Bracketed vs (DisplayServer, Ptr Backend) a
getLinuxDMABufBracket = Bracketed (liftIO . uncurry createDMABuf) (liftIO . destroyDMABuf)
