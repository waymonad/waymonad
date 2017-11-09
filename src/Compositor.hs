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
module Compositor
where

import Foreign.Ptr (Ptr)
import Graphics.Wayland.Server (DisplayServer)

import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.DeviceManager (WlrDeviceManager)
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.WlRoots.Render (Renderer)
import Graphics.Wayland.WlRoots.Screenshooter (WlrScreenshooter)
import Graphics.Wayland.WlRoots.Shell (WlrShell)

import Input (Input)
import XWayland (XWayShell)
import XdgShell (XdgShell)


data Compositor = Compositor
    { compDisplay :: DisplayServer
    , compRenderer :: Ptr Renderer
    , compCompositor :: Ptr WlrCompositor
    , compShell :: Ptr WlrShell
    , compXdg :: XdgShell
    , compManager :: Ptr WlrDeviceManager
    , compXWayland :: XWayShell
    , compBackend :: Ptr Backend
    , compLayout :: Ptr WlrOutputLayout
    , compInput :: Input
    , compScreenshooter :: Ptr WlrScreenshooter
    }
