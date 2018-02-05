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
{-|
Module      : Startup.Generic
Description : Allows to run any Way action when the compositor is done starting up
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module Waymonad.Actions.Startup.Generic
    ( getStartupBracket
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Graphics.Wayland.Server (DisplayServer, eventLoopAddIdle, displayGetEventLoop)

import Waymonad.Start (Bracketed (..))
import Waymonad (unliftWay)
import Waymonad.Types (Way)

{- | Run a Way action when the compositor is started up.

@
    getStartupBracket (spawn "alacritty")
@
-}
getStartupBracket :: Way vs a () -> Bracketed vs DisplayServer a
getStartupBracket act = Bracketed (\dsp -> do
        evtLoop <- liftIO $ displayGetEventLoop dsp
        cb <- unliftWay act
        void . liftIO $ eventLoopAddIdle evtLoop cb
    ) (const $ pure ())
