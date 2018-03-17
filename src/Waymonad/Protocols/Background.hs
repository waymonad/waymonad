{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
module Waymonad.Protocols.Background
where

import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.C.Types (CInt (..))

import Graphics.Wayland.WlRoots.Surface (WlrSurface, surfaceFromResource, getSurfaceResource)
import Graphics.Wayland.WlRoots.Output (WlrOutput, outputFromResource, outputResourceForClient)

import Graphics.Wayland.Global (WlGlobal)
import Graphics.Wayland.Resource (WlResource, resourceDestroy)
import Graphics.Wayland.Server (DisplayServer (..), Client (..))
import "waymonad-scanner" Graphics.Wayland.Scanner
import "waymonad-scanner" Graphics.Wayland.Scanner.WLS
import "waymonad-scanner" Graphics.Wayland.Scanner.Dispatcher (WlInterface, createResource)

import Waymonad.Start (Bracketed (..))

import qualified Data.Map as M
import qualified Language.Haskell.TH as TH

runScanner (protocolFromFile "protocols/background.xml") $ ScannerEnv $ M.fromList
    [ ("wl_surface", (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlrSurface), TH.VarE 'surfaceFromResource, TH.LamE [TH.WildP] (TH.VarE 'getSurfaceResource)))
    , ("wl_output", (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlrOutput), TH.VarE 'outputFromResource, TH.VarE 'outputResourceForClient))
    ]

type BindFunc a = Ptr Client -> Ptr a -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "wl_global_create" c_create :: Ptr DisplayServer -> Ptr WlInterface -> CInt -> Ptr a -> FunPtr (BindFunc a) -> IO (Ptr WlGlobal)
foreign export ccall "bind_background" bindBackground :: BindFunc ()
foreign import ccall safe "&bind_background" bindBackgroundPtr :: FunPtr (BindFunc ())

addBackgroundSurface :: Word32 -> Ptr WlrSurface -> IO ()
addBackgroundSurface = error "Tried to add a background surface o.0"

backgroundHandler :: Ptr WlResource -> ZBackgroundRequests
backgroundHandler res = ZBackgroundRequests
    { zBackgroundRequestDestroy = resourceDestroy res
    , zBackgroundRequestGetBackgroundSurface = addBackgroundSurface
    }

-- | This should create a zBackground wl_resource
bindBackground :: Ptr Client -> Ptr a -> Word32 -> Word32 -> IO ()
bindBackground client _ version rsid = do
    rs <- createResource (Client client) zBackgroundInterface (fromIntegral version) rsid
    setZBackgroundDispatcher rs (backgroundHandler rs) (pure ())


makeBackgroundGlobal :: DisplayServer -> IO (Ptr WlGlobal)
makeBackgroundGlobal (DisplayServer dsp) =
    c_create dsp (zBackgroundInterface) 1 nullPtr bindBackgroundPtr


getBackgroundBracket :: Bracketed vs DisplayServer a
getBackgroundBracket = Bracketed (liftIO . makeBackgroundGlobal) (const $ pure ())
