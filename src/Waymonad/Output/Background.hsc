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
module Waymonad.Output.Background
    ( getLogoBackground
    , BackgroundTexture (..)
    , loadLogoBackground
    )
where

#include <wayland-server.h>

import Control.Monad.IO.Class (liftIO)

import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Render (Texture, Renderer, textureCreate, uploadPixels)

import Waymonad (getState)
import Waymonad.Types
import Waymonad.Utility.Extensible (setEState)

foreign import ccall unsafe "get_logo_data" c_get_logo :: Ptr a

data BackgroundTexture = BackgroundTexture
    { backgroundTexture :: Ptr Texture
    , backgroundWidth :: Int
    , backgroundHeight :: Int
    }

getLogoBackground :: Ptr Renderer -> IO BackgroundTexture
getLogoBackground renderer = do
    texture <- textureCreate renderer
    uploadPixels texture #{const WL_SHM_FORMAT_ABGR8888} 512 512 512 c_get_logo
    pure $ BackgroundTexture texture 512 512

loadLogoBackground :: Way vs ws ()
loadLogoBackground = do
    WayBindingState { wayCompositor = Compositor {compRenderer = renderer} }<- getState

    setEState . Just =<< liftIO (getLogoBackground renderer)
