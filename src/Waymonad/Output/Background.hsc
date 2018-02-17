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
