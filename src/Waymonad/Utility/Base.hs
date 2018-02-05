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
module Waymonad.Utility.Base
    ( intToPtr
    , ptrToInt
    , whenJust
    , doJust

    , These (..)
    , getThis
    , getThat

    , firstDir

    , showT
    , readT
    )
where

import Data.Default (Default (..))
import Data.Text (Text)
import Foreign.Ptr (Ptr, ptrToIntPtr, intPtrToPtr)
import Safe (readMay)

import qualified Data.Text as T

intToPtr :: Integral a => a -> Ptr b
intToPtr = intPtrToPtr . fromIntegral

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

whenJust :: (Applicative m, Default r) => Maybe a -> (a -> m r) -> m r
whenJust Nothing _ = pure def
whenJust (Just x) f = f x

doJust :: (Monad m, Default r) => m (Maybe a) -> (a -> m r) -> m r
doJust val act = flip whenJust act =<< val

data These a = This a | That a | These a a
    deriving (Eq, Show, Read)

instance Functor These where
    fmap f (This a) = This $ f a
    fmap f (That a) = That $ f a
    fmap f (These a b) = These (f a) (f b)

getThis :: These a -> Maybe a
getThis (This x) = Just x
getThis (These x _) = Just x
getThis _ = Nothing

getThat :: These a -> Maybe a
getThat (That x) = Just x
getThat (These _ x) = Just x
getThat _ = Nothing

firstDir :: String -> (String, String)
firstDir [] = ([], [])
firstDir path = span (/= '/') path

showT :: Show a => a -> Text
showT = T.pack . show

readT :: Read a => Text -> Maybe a
readT = readMay . T.unpack
