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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Waymonad.Extensible
    ( StateMap
      , ExtensionClass (..)

    , getValue
    , setValue
    , modifyValue
    )
where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, typeOf, cast)

import qualified Data.Map.Strict as M

newtype StateMap = StateMap (Map String StateExtension)
    deriving (Monoid)

class Typeable a => ExtensionClass a where
    initialValue :: a

data StateExtension = forall a. ExtensionClass a => StateExtension a

getName :: Typeable a => a -> String
getName = show . typeOf

getValue :: forall a. ExtensionClass a => StateMap -> a
getValue (StateMap state) = fromMaybe initialValue $ do
    (StateExtension x) <- M.lookup (getName (undefined :: a)) state
    cast x

setValue :: ExtensionClass a => a -> StateMap -> StateMap
setValue val (StateMap state) = StateMap $ M.insert (getName val) (StateExtension val) state

modifyValue :: ExtensionClass a => (a -> a) -> StateMap -> StateMap
modifyValue fun state = setValue (fun $ getValue state) state
