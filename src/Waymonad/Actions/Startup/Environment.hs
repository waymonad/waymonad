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
Module      : Startup.Environment
Description : Allows to set environment variables on startup.
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module Waymonad.Actions.Startup.Environment
where

import Control.Monad.IO.Class (liftIO)
import System.Environment (setEnv)

import Graphics.Wayland.Server (DisplayServer)

import Waymonad.Start (Bracketed (..))

{- | Set environment variables on startup.

This happens early enough to be used by wlroots/backend stuff.

@
    envBracket [(\"PULSE_SERVER\", "zelda.ongy")]
@
-}
envBracket :: [(String, String)] -> Bracketed vs DisplayServer ws
envBracket xs = Bracketed
    (\_ -> liftIO (mapM_ (uncurry setEnv) xs))
    (\_ -> pure ())
