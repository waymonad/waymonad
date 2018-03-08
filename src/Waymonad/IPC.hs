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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Waymonad.IPC
where

import Data.Default
import Control.Applicative ((<|>))
import Foreign.C.Error (Errno, eOK, eINVAL)
import Data.Text (Text)
import Data.Typeable (Typeable, TypeRep, cast, typeRep, Proxy (..))

import Waymonad.Types (Way)
import Waymonad.Utility.Base (showT, readT)

data IPCRead vs ws = forall a. Typeable a => IPCRead
    { entryRType :: TypeRep
    , entryRFun  :: Way vs ws (Either Errno a)
    }


simpleIPCRead :: forall vs ws a. Typeable a => Way vs ws a -> IPCRead vs ws
simpleIPCRead fun = IPCRead
    { entryRType = typeRep (Proxy :: Proxy a)
    , entryRFun = Right <$> fun
    }

textifyIPCRead :: Show a => Way vs ws a -> IPCRead vs ws
textifyIPCRead fun = simpleIPCRead (fmap showT $ fun)



simpleIPCWrite :: forall vs ws a. Typeable a => (a -> Way vs ws ()) -> IPCWrite vs ws
simpleIPCWrite fun = IPCWrite
    { entryWType = typeRep (Proxy :: Proxy a)
    , entryWFun = \v -> fun v >> pure eOK
    }

textifyIPCWrite :: Read a => (a -> Way vs ws ()) -> IPCWrite vs ws
textifyIPCWrite fun = IPCWrite
    { entryWType = typeRep (Proxy :: Proxy Text)
    , entryWFun = \v -> case readT v of
        Just x -> fun x >> pure eOK
        Nothing -> pure eINVAL
    }


data IPCWrite vs ws = forall a. Typeable a => IPCWrite
    { entryWType :: TypeRep
    , entryWFun  :: a -> Way vs ws Errno
    }

data IPCEntry vs ws = IPCEntry
    { ipcEntryRead  :: [IPCRead vs ws]
    , ipcEntryWrite :: [IPCWrite vs ws]
    , ipcEntryReadable  :: Way vs ws Bool
    , ipcEntryWriteable :: Way vs ws Bool
    }


newtype IPCGroup vs ws = IPCGroup [(Text, Either (IPCGroup vs ws) (IPCEntry vs ws))]
    deriving (Default, Monoid)

getEntryReadFun :: forall vs ws a. (Typeable vs, Typeable ws, Typeable a)
                => IPCEntry vs ws -> Maybe (Way vs ws (Either Errno a))
getEntryReadFun IPCEntry { ipcEntryRead = candidates } =
    foldr (<|>) Nothing $ fmap getFun candidates
    where   getFun :: IPCRead vs ws -> Maybe (Way vs ws (Either Errno a))
            getFun IPCRead { entryRFun = fun } = cast fun

getEntryWriteFun :: forall vs ws a. (Typeable vs, Typeable ws, Typeable a)
                 => IPCEntry vs ws -> Maybe (a -> Way vs ws Errno)
getEntryWriteFun IPCEntry { ipcEntryWrite = candidates } =
    foldr (<|>) Nothing $ fmap getFun candidates
    where   getFun :: IPCWrite vs ws -> Maybe (a -> Way vs ws Errno)
            getFun IPCWrite { entryWFun = fun } = cast fun


