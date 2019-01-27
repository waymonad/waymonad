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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Waymonad.Tabletv2
    ( getTabletv2Bracket
    , getManager
    )
where

import Control.Monad.IO.Class (liftIO)

import Graphics.Wayland.Server (DisplayServer)
import Graphics.Wayland.WlRoots.Global

import Waymonad.Utility.Extensible
import Waymonad.Start (Bracketed (..))
import Waymonad.GlobalFilter
import Waymonad.Types (Way)
import Waymonad.GlobalFilter (registerGlobal)

import qualified Graphics.Wayland.WlRoots.Tabletv2 as R

makeManager :: DisplayServer -> Way vs a R.TabletManagerv2
makeManager dsp = do
    ret <- liftIO $ R.createTabletManagerv2 dsp
    registerGlobal "Tabletv2" =<< (liftIO $ getGlobal ret)
    setEState $ Just ret
    pure ret

getTabletv2Bracket :: Bracketed vs DisplayServer a
getTabletv2Bracket = Bracketed makeManager (liftIO . removeGlobal)

getManager :: Way vs ws (Maybe R.TabletManagerv2)
getManager = getEState
