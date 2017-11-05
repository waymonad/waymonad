{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Waymonad.Extensible
    ( StateMap
    , ExtensionClass

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
