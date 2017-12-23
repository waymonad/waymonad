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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module XWayland
    ( xwayShellCreate
    , XWayShell
    , overrideXRedirect
    )
where

import System.IO
import Foreign.Ptr (Ptr, ptrToIntPtr)

import Control.Monad.Reader (ask)
import Data.Maybe (fromJust)
import WayUtil (setSignalHandler)
import Foreign.Storable (Storable(..))
import qualified Graphics.Wayland.WlRoots.XWayland as X
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.Box (Point(..), WlrBox(..), boxContainsPoint)
import Data.IORef (newIORef, IORef, modifyIORef, readIORef)
import Graphics.Wayland.Server (DisplayServer)
import Control.Monad.IO.Class

import Managehook
import Waymonad
import View
import WayUtil.Log (logPutText)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )

import qualified Data.IntMap.Strict as M
import Data.IntMap (IntMap)

type MapRef =  IORef (IntMap View)

data XWayShell = XWayShell
    { xwaySurfaceRef :: MapRef
    , xwayWlrootsShell :: Ptr X.XWayland
    }


data XWaySurface = XWaySurface
    { _surfXWay :: Ptr X.XWayland
    , unXway :: Ptr X.X11Surface
    }

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr


xwayShellCreate
    :: DisplayServer
    -> Ptr WlrCompositor
    -> (View -> Way a ())
    -> (View -> Way a ())
    -> Way a XWayShell
xwayShellCreate display comp addFun delFun = do
    surfaces <- liftIO $ newIORef mempty
    roots <- liftIO $ X.xwaylandCreate display comp

    setCallback (handleXwaySurface roots surfaces addFun delFun) (X.xwayBindNew roots)

    pure $ XWayShell
        { xwaySurfaceRef = surfaces
        , xwayWlrootsShell = roots
        }

handleXwayDestroy
    :: MapRef
    -> (View -> Way a ())
    -> Ptr X.X11Surface
    -> Way a ()
handleXwayDestroy ref delFun surf = do
    view <- fromJust . M.lookup (ptrToInt surf) <$> liftIO (readIORef ref)
    triggerViewDestroy view
    delFun view

    liftIO $ do
        modifyIORef ref $ M.delete (ptrToInt surf)
        sptr :: Ptr () <- peek (X.getX11SurfaceDataPtr surf)
        freeStablePtr $ castPtrToStablePtr sptr

handleXwaySurface
    :: Ptr X.XWayland
    -> MapRef
    -> (View -> Way a ())
    -> (View -> Way a ())
    -> Ptr X.X11Surface
    -> Way a ()
handleXwaySurface xway ref addFun delFun surf = do
    let xwaySurf = XWaySurface xway surf
    logPutText loggerX11 "New XWayland surface"
    view <- createView xwaySurf
    addFun view

    liftIO $ do
        modifyIORef ref $ M.insert (ptrToInt surf) view
        activate xwaySurf True

    let signals = X.getX11SurfaceEvents surf

    handler <- setSignalHandler (X.x11SurfaceEvtDestroy signals) $ handleXwayDestroy ref delFun
    handler2 <- setSignalHandler (X.x11SurfaceEvtType signals) $ (const $ liftIO $ hPutStrLn stderr "Some surface set type")

    liftIO $ do
        sptr <- newStablePtr (handler, handler2)
        poke (X.getX11SurfaceDataPtr surf) (castStablePtrToPtr sptr)


instance ShellSurface XWaySurface where
    close (XWaySurface _ surf) = liftIO $ X.xwayCloseSurface surf
    getSurface = liftIO . X.xwaySurfaceGetSurface . unXway
    getSize (XWaySurface _ surf) = liftIO $ do
        box <- X.getX11SurfaceGeometry surf
        pure (fromIntegral $ boxWidth box, fromIntegral $ boxHeight box)
    resize (XWaySurface _ surf) width height = liftIO $ do
        p@(Point x y) <- X.getX11SurfacePosition surf
        hPutStrLn stderr $ show p
        X.configureX11Surface surf
            (fromIntegral x) (fromIntegral y)
            (fromIntegral width) (fromIntegral height)
        hPutStrLn stderr "Survived!"
    activate (XWaySurface _ surf) active = liftIO $ X.activateX11Surface surf active
    getEventSurface (XWaySurface _ surf) x y = liftIO $ do
        (WlrBox _ _ w h) <- X.getX11SurfaceGeometry surf
        if boxContainsPoint (Point (floor x) (floor y)) (WlrBox 0 0 w h)
           then do
                ret <- X.xwaySurfaceGetSurface surf
                pure $ Just (ret, x, y)
            else pure Nothing
    setPosition (XWaySurface _ surf) x y =
        let point = Point (floor x) (floor y)
         in liftIO $ X.setX11SurfacePosition surf point
    getID (XWaySurface _ surf) = ptrToInt surf
    getTitle = liftIO . X.getTitle . unXway
    getAppId = liftIO . X.getClass . unXway


overrideXRedirect :: Managehook a
overrideXRedirect = do
    view <- ask
    case getViewInner view of
        Nothing -> mempty
        Just (XWaySurface _ surf) -> do
            override <- liftIO $ X.x11SurfaceOverrideRedirect surf
            if override
                then liftIO $ do
                    hPutStrLn stderr "Overriding a redirect"
                    (Point x y) <- X.getX11SurfacePosition surf
                    (width, height) <- getViewSize view
                    pure . InsertFloating $ WlrBox x y (floor width) (floor height)
                else mempty
