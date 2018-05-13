module Waymonad.Protocols.InputInhibit
where

import Foreign.Ptr (Ptr)

import Graphics.Wayland.Server (Client)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)

import Waymonad.Types (Way)

isInhibited :: Ptr WlrSurface -> Way vs ws Bool
getInhibitingClient :: Way vs ws (Maybe Client)
