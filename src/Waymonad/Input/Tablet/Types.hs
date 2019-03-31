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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Input.Tablet.Types
where

import Data.IORef (IORef)
import Graphics.Wayland.Signal (removeListener, ListenerToken)

import qualified Graphics.Wayland.WlRoots.Tabletv2 as R

data Tablet = Tablet
    { tabRoots :: R.Tabletv2
    , tabTockens :: [ListenerToken]
    }

data TabletTool = TabletTool
    { toolRoots :: R.TabletToolv2
    , toolTockens :: [ListenerToken]
    , toolOutputToken :: IORef Int
    }

instance Eq Tablet where
    Tablet { tabRoots = l } == Tablet { tabRoots = r } = l == r

instance Ord Tablet where
    Tablet { tabRoots = l } `compare` Tablet { tabRoots = r } = l `compare` r

data TabletPad = TabletPad
    { padRoots  :: R.TabletPadv2
    , padTockens :: [ListenerToken]
    , padTablet  :: IORef (Maybe Tablet)
    }

instance Eq TabletPad where
    TabletPad { padRoots = l } == TabletPad { padRoots = r } = l == r

instance Ord TabletPad where
    TabletPad { padRoots = l } `compare` TabletPad { padRoots = r } = l `compare` r
