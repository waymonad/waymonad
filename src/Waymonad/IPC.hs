{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Waymonad.IPC
where

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


data IPCGroup vs ws = IPCGroup [(Text, Either (IPCGroup vs ws) (IPCEntry vs ws))]

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


