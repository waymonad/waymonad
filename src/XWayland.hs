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
{-# LANGUAGE TupleSections #-}
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

import Control.Monad.Reader (ask, when)
import Data.IORef (newIORef, IORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import WayUtil.Signal (setSignalHandler)
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
import ViewSet (WSTag)
import WayUtil.Log (logPutText, LogPriority (..))
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
    :: WSTag a
    => DisplayServer
    -> Ptr WlrCompositor
    -> Way a XWayShell
xwayShellCreate display comp = do
    surfaces <- liftIO $ newIORef mempty
    roots <- liftIO $ X.xwaylandCreate display comp

    setCallback (handleXwaySurface roots surfaces) (X.xwayBindNew roots)

    pure $ XWayShell
        { xwaySurfaceRef = surfaces
        , xwayWlrootsShell = roots
        }

handleXwayDestroy
    :: WSTag a
    => MapRef
    -> Ptr X.X11Surface
    -> Way a ()
handleXwayDestroy ref surf = do
    logPutText loggerX11 Debug "Destroying XWayland surface"
    view <- fromJust . M.lookup (ptrToInt surf) <$> liftIO (readIORef ref)
    triggerViewDestroy view
    removeView view

    liftIO $ do
        modifyIORef ref $ M.delete (ptrToInt surf)
        sptr :: Ptr () <- peek (X.getX11SurfaceDataPtr surf)
        freeStablePtr $ castPtrToStablePtr sptr

handleX11Configure :: View -> IORef (Int, Int) -> Ptr X.ConfigureEvent -> Way a ()
handleX11Configure view ref evt = do
    logPutText loggerX11 Debug "Got configure request"
    liftIO $ do
        event <- peek evt
        (oldWidth, oldHeight) <- readIORef ref
        let width = fromIntegral $ X.configureEvtWidth event
        let height = fromIntegral $ X.configureEvtHeight event
        when (oldWidth /= width || oldHeight /= height) $ do
            setViewLocal view $ WlrBox 0 0 width height
            writeIORef ref (width, height)

handleXwaySurface
    :: WSTag a
    => Ptr X.XWayland
    -> MapRef
    -> Ptr X.X11Surface
    -> Way a ()
handleXwaySurface xway ref surf = do
    let xwaySurf = XWaySurface xway surf
    logPutText loggerX11 Debug "New XWayland surface"
    view <- createView xwaySurf
    insertView view

    liftIO $ do
        modifyIORef ref $ M.insert (ptrToInt surf) view
        activate xwaySurf True

    let signals = X.getX11SurfaceEvents surf

    sizeRef <- liftIO $ newIORef (0, 0)
    handler <- setSignalHandler (X.x11SurfaceEvtDestroy signals) $ handleXwayDestroy ref
    handler2 <- setSignalHandler (X.x11SurfaceEvtType signals) $ (const $ liftIO $ hPutStrLn stderr "Some surface set type")
    handler3 <- setSignalHandler (X.x11SurfaceEvtConfigure signals) $ handleX11Configure view sizeRef

    liftIO $ do
        sptr <- newStablePtr (handler, handler2, handler3)
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
                pure $ fmap (,x, y) ret
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
                then do
                    liftWay $ logPutText loggerX11 Info "Overriding a redirect"
                    (Point x y) <- liftIO $ X.getX11SurfacePosition surf
                    (width, height) <- getViewSize view
                    pure . InsertFloating $ WlrBox x y (floor width) (floor height)
                else mempty
