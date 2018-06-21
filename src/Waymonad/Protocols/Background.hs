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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
module Waymonad.Protocols.Background
where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, FunPtr, castPtr)
import Foreign.StablePtr (deRefStablePtr, castPtrToStablePtr, newStablePtr, castStablePtrToPtr)

import Graphics.Wayland.WlRoots.Box (Point (..), WlrBox (..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface, surfaceFromResource, getSurfaceResource, surfaceGetSize)
import Graphics.Wayland.WlRoots.Output (WlrOutput, outputFromResource, outputResourceForClient, outputTransformedResolution)

import Graphics.Wayland.Global (WlGlobal)
import Graphics.Wayland.Resource (WlResource, resourceDestroy, resourceGetClient, addResourceDestroyListener)
import Graphics.Wayland.Server (DisplayServer (..), Client (..))
import "waymonad-scanner" Graphics.Wayland.Scanner
import "waymonad-scanner" Graphics.Wayland.Scanner.WLS
import "waymonad-scanner" Graphics.Wayland.Scanner.Dispatcher (WlInterface, createResource)

import Waymonad (makeCallback, makeCallback2, getState, runWay)
import Waymonad.Utility.LayerCache (getLayerContent, setLayerContent)
import Waymonad.Start (Bracketed (..))
import Waymonad.Utility (getOutputs)
import Waymonad.View (createView, resizeView)
import Waymonad.Types (Way, Output(outputRoots), SSDPrio (NoSSD))
import Waymonad.Types.Core (ShellSurface (..))

import qualified Data.Map as M
import qualified Language.Haskell.TH as TH


runScanner (protocolFromFile "protocols/background.xml") $ ScannerEnv $ M.fromList
    [ ("wl_surface", (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlrSurface), TH.VarE 'surfaceFromResource, TH.LamE [TH.WildP] (TH.VarE 'getSurfaceResource)))
    , ("wl_output", (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlrOutput), TH.VarE 'outputFromResource, TH.VarE 'outputResourceForClient))
    ]

type BindFunc a = Ptr Client -> Ptr a -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "wl_global_create" c_create :: Ptr DisplayServer -> Ptr WlInterface -> CInt -> Ptr a -> FunPtr (BindFunc a) -> IO (Ptr WlGlobal)
foreign export ccall "bind_background" bindBackground :: BindFunc (vs, ws)
foreign import ccall safe "&bind_background" bindBackgroundPtr :: FunPtr (BindFunc a)

data BackgroundSurface = BackgroundSurface
    { backSurfResource :: Ptr WlResource
    , backSurfSurface  :: Ptr WlrSurface
    }

instance ShellSurface BackgroundSurface where
    getSurface BackgroundSurface {backSurfSurface = surf} = pure  $ Just surf
    getSize BackgroundSurface {backSurfSurface = surf} = liftIO $ do
        Point w h <- surfaceGetSize surf
        pure (fromIntegral w, fromIntegral h)
    resize _ _ _ _ = pure False
    activate _ _ = pure ()
    close BackgroundSurface {backSurfResource = rs} = liftIO $ zBackgroundSurfacePostRemove rs
    getEventSurface BackgroundSurface {backSurfSurface = surf} x y = liftIO $ do
        Point w h <- surfaceGetSize surf
        pure $ if x < fromIntegral w && y < fromIntegral h
            then Just (surf, x, y)
            else Nothing
    getID _ = 0
    getTitle _ = pure Nothing
    getAppId _ = pure Nothing
    takesFocus _ _ = pure False
    hasCSD _ = pure False


findSuitableOutputs :: Way vs ws [Output]
findSuitableOutputs = do
    outputs <- getOutputs
    filterM (fmap null . getLayerContent "background") outputs


addBackgroundSurface :: Ptr WlResource -> Word32 -> Ptr WlrSurface -> Way vs ws ()
addBackgroundSurface global rsid surf = do
    client <- liftIO $ resourceGetClient global
    rs <- liftIO $ createResource client zBackgroundSurfaceInterface 1 rsid
    outM <- findSuitableOutputs
    case outM of
        [] -> liftIO $ do
            zBackgroundSurfacePostRemove rs
            resourceDestroy rs
        (out:_) -> do
            destroyCB <- makeCallback $ \_ -> setLayerContent "background" out []
            liftIO $ zBackgroundSurfacePostSetOutput rs (outputRoots out)
            let bs = BackgroundSurface rs surf
            Point w h <- liftIO $ outputTransformedResolution $ outputRoots out

            v <- createView bs
            _ <- resizeView v (fromIntegral w) (fromIntegral h) (pure ())
            setLayerContent "background" out [(v, NoSSD mempty, WlrBox 0 0 w h)]
            liftIO $ addResourceDestroyListener rs destroyCB

backgroundHandler :: Ptr WlResource -> Way vs ws ZBackgroundRequests
backgroundHandler res = do
    addCB <- makeCallback2 $ addBackgroundSurface res
    pure ZBackgroundRequests
        { zBackgroundRequestDestroy = resourceDestroy res
        , zBackgroundRequestGetBackgroundSurface = addCB
        }

-- Abuse the pointers type here to get the type over the callback boundary
-- | This should create a zBackground wl_resource
bindBackground :: forall vs ws. Ptr Client -> Ptr (vs, ws) -> Word32 -> Word32 -> IO ()
bindBackground client tPtr version rsid = do
    let sPtr = castPtr tPtr
    runner :: Way vs ws ZBackgroundRequests -> IO ZBackgroundRequests <- deRefStablePtr $ castPtrToStablePtr sPtr
    rs <- createResource (Client client) zBackgroundInterface (fromIntegral version) rsid
    handlers <- runner (backgroundHandler rs)
    setZBackgroundDispatcher rs handlers (pure ())

    -- Hurr Durr derp
    runner2 :: Way vs ws [Output] -> IO [Output] <- deRefStablePtr $ castPtrToStablePtr sPtr
    outs <- runner2 findSuitableOutputs
    mapM_ (\_ -> zBackgroundPostCreateBackground rs) outs


makeBackgroundGlobal :: forall vs ws. DisplayServer -> Way vs ws ((Ptr WlGlobal, Ptr (vs, ws)))
makeBackgroundGlobal (DisplayServer dsp) = do
    state <- getState
    sPtr :: Ptr (vs, ws) <- liftIO $ castPtr . castStablePtrToPtr <$> newStablePtr (runWay state :: Way vs ws a -> IO a)
    ret <- liftIO $ c_create dsp zBackgroundInterface 1 sPtr bindBackgroundPtr
    pure (ret, sPtr)


getBackgroundBracket :: Bracketed vs DisplayServer a
getBackgroundBracket = Bracketed (makeBackgroundGlobal) (const $ pure ())
