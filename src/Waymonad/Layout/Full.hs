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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Waymonad.Layout.Full
where

import Control.Applicative ((<|>))
import Waymonad.ViewSet

import Waymonad.Types (SSDPrio (..))

data Full = Full

instance LayoutClass Full where
    description _ = "Full"
    handleMessage _ _ _ = Nothing
    broadcastMessage _ _ = Nothing

instance FocusCore vs ws => GenericLayoutClass Full vs ws where
    pureLayout _ vs ws box = case _getFocused vs ws Nothing <|> getFirst vs ws of
        Nothing -> []
        Just v -> [(v, NoSSD mempty, box)]
