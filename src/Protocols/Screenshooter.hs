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
{-# LANGUAGE OverloadedStrings #-}
module Protocols.Screenshooter
    ( getScreenshooterBracket
    )
where

import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Screenshooter

import Shared (Bracketed (..))
import GlobalFilter
import Waymonad (getState)
import Waymonad.Types (Way, WayBindingState (..), Compositor (..))

makeManager :: () -> Way vs a (Ptr WlrScreenshooter)
makeManager _ = do
    Compositor {compDisplay = display, compRenderer = renderer} <- wayCompositor <$> getState
    ptr <- liftIO $ screenshooterCreate display renderer
    registerGlobal "Screenshooter" =<< liftIO (getScreenshooterGlobal ptr)
    pure ptr

getScreenshooterBracket :: Bracketed vs () a
getScreenshooterBracket = Bracketed makeManager (liftIO . screenshooterDestroy)

