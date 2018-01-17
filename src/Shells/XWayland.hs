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
module Shells.XWayland
    ( xwayShellCreate
    , XWayShell
    , overrideXRedirect

    , XWayRef
    , makeShell
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask, when)
import Data.IORef (newIORef, IORef, readIORef, writeIORef, modifyIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
    ( castPtrToStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , newStablePtr
    )
import Foreign.Storable (Storable(..))
import System.IO

import Graphics.Wayland.Server (DisplayServer)
import Graphics.Wayland.Signal (removeListener, ListenerToken)
import Graphics.Wayland.WlRoots.Box (Point(..), WlrBox(..), boxContainsPoint)
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.Surface (WlrSurface, WlrSurfaceEvents (..), getWlrSurfaceEvents)

import Input.Seat
import Managehook
import Utility
import View
import ViewSet (WSTag, FocusCore)
import WayUtil.Log (logPutText, LogPriority (..))
import WayUtil.Signal (setSignalHandler, setDestroyHandler)
import Waymonad
import Waymonad.Types
    ( Compositor (..)
    , ShellClass (..)
    , WayBindingState (..)
    , WayShell (..)
    )

import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Graphics.Wayland.WlRoots.XWayland as X

newtype XWayRef = XWayRef (IORef (Maybe XWayShell))

makeShell :: IO WayShell
makeShell = WayShell . XWayRef <$> newIORef Nothing

instance ShellClass XWayRef where
    activateShell (XWayRef ref) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just _ -> pure ()
            Nothing -> do
                dsp <- compDisplay . wayCompositor <$> getState
                comp <- compCompositor . wayCompositor <$> getState
                shell <- xwayShellCreate dsp comp
                liftIO $ writeIORef ref $ Just shell
    deactivateShell (XWayRef ref) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just XWayShell {xwayWlrootsShell = roots} -> liftIO $ do
                X.xwaylandDestroy roots
                writeIORef ref Nothing
            Nothing -> pure ()
    isShellActive (XWayRef ref) = do
        ret <- liftIO $ readIORef ref
        pure $ case ret of
            Just _ -> True
            Nothing -> False
    getShellName _ = "XWayland"
    getShellViews (XWayRef ref) = liftIO $ do
        ret <- readIORef ref
        case ret of
            Nothing -> pure mempty
            Just XWayShell {xwaySurfaceRef = surfRef} -> do
                surfMap <- readIORef surfRef
                pure $ S.fromList $ M.elems surfMap

type MapRef =  IORef (IntMap View)

data XWayShell = XWayShell
    { xwaySurfaceRef :: MapRef
    , xwayWlrootsShell :: Ptr X.XWayland
    }


data XWaySurface = XWaySurface
    { _surfXWay :: Ptr X.XWayland
    , unXway :: Ptr X.X11Surface
    }


xwayShellCreate
    :: (FocusCore vs a, WSTag a)
    => DisplayServer
    -> Ptr WlrCompositor
    -> Way vs a XWayShell
xwayShellCreate display comp = do
    surfaces <- liftIO $ newIORef mempty
    roots <- liftIO $ X.xwaylandCreate display comp

    setCallback (handleXwaySurface roots surfaces) (X.xwayBindNew roots)

    pure XWayShell
        { xwaySurfaceRef = surfaces
        , xwayWlrootsShell = roots
        }

handleXwayDestroy
    :: (FocusCore vs a, WSTag a)
    => MapRef
    -> [ListenerToken]
    -> Ptr X.X11Surface
    -> Way vs a ()
handleXwayDestroy ref tokens surf = do
    logPutText loggerX11 Debug "Destroying XWayland surface"
    view <- fromJust . M.lookup (ptrToInt surf) <$> liftIO (readIORef ref)
    triggerViewDestroy view
    removeView view
    liftIO $ do
        modifyIORef ref (M.delete $ ptrToInt surf)
        mapM_ removeListener tokens
        stPtr <- peek (X.getX11SurfaceDataPtr surf)
        freeStablePtr $ castPtrToStablePtr stPtr

handleX11Configure :: View -> Ptr X.X11Surface -> Ptr X.ConfigureEvent -> Way vs a ()
handleX11Configure view surf evt = do
    logPutText loggerX11 Debug "Got configure request"
    override <- liftIO $ X.x11SurfaceOverrideRedirect surf
    when override $ do
        event <- liftIO $ peek evt
        let width = fromIntegral $ X.configureEvtWidth event
        let height = fromIntegral $ X.configureEvtHeight event
        let x = fromIntegral $ X.configureEvtX event
        let y = fromIntegral $ X.configureEvtY event
        setViewBox view (WlrBox x y width height)

handleX11Map :: (FocusCore vs ws, WSTag ws) => View -> Ptr X.X11Surface -> Way vs ws ()
handleX11Map view surf = do
    Point x y <- liftIO $ X.getX11SurfacePosition surf
    moveView view (fromIntegral x) (fromIntegral y)
    insertView view
    doJust getSeat (void . flip keyboardEnter view)

handleOverrideCommit :: View -> Ptr X.X11Surface -> Ptr WlrSurface -> Way vs a ()
handleOverrideCommit view surf _ = do
    WlrBox _ _ w h <- liftIO $ X.getX11SurfaceGeometry surf
    resizeView view (fromIntegral w) (fromIntegral h)

handleXwaySurface
    :: (FocusCore vs a, WSTag a)
    => Ptr X.XWayland
    -> MapRef
    -> Ptr X.X11Surface
    -> Way vs a ()
handleXwaySurface xway ref surf = do
    let xwaySurf = XWaySurface xway surf
    logPutText loggerX11 Debug "New XWayland surface"
    view <- createView xwaySurf
    insertView view

    liftIO $ do
        modifyIORef ref $ M.insert (ptrToInt surf) view
        activate xwaySurf True

    let signals = X.getX11SurfaceEvents surf

    h1 <- setSignalHandler (X.x11SurfaceEvtType signals) $ const $ liftIO $ hPutStrLn stderr "Some surface set type"
    h3 <- setSignalHandler (X.x11SurfaceEvtMove signals) $ const . liftIO $ (hPutStrLn stderr "Something requests a move")
    h4 <- setSignalHandler (X.x11SurfaceEvtConfigure signals) $ handleX11Configure view surf
    h5 <- setSignalHandler (X.x11SurfaceEvtUnmap signals) $ const (removeView view)
    h6 <- setSignalHandler (X.x11SurfaceEvtMap signals) $ handleX11Map view

    setDestroyHandler (X.x11SurfaceEvtDestroy signals) $ handleXwayDestroy ref [h1, h3, h4, h5, h6]
    liftIO $ do
        stPtr <- newStablePtr view
        poke (X.getX11SurfaceDataPtr surf) (castStablePtrToPtr stPtr)


instance ShellSurface XWaySurface where
    close (XWaySurface _ surf) = liftIO $ X.xwayCloseSurface surf
    getSurface = liftIO . X.xwaySurfaceGetSurface . unXway
    getSize (XWaySurface _ surf) = liftIO $ do
        box <- X.getX11SurfaceGeometry surf
        pure (fromIntegral $ boxWidth box, fromIntegral $ boxHeight box)
    resize (XWaySurface _ surf) width height = liftIO $ do
        (Point x y) <- X.getX11SurfacePosition surf
        X.configureX11Surface surf
            (fromIntegral x) (fromIntegral y)
            (fromIntegral width) (fromIntegral height)
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


overrideXRedirect :: Managehook vs a
overrideXRedirect = do
    view <- ask
    case getViewInner view of
        Nothing -> mempty
        Just (XWaySurface _ surf) -> do
            override <- liftIO $ X.x11SurfaceOverrideRedirect surf
            if override
                then do
                    liftWay $ logPutText loggerX11 Info "Overriding a redirect"
                    liftWay $ doJust (liftIO $ X.xwaySurfaceGetSurface surf) $ \wlrSurf -> do
                        let events = getWlrSurfaceEvents wlrSurf
                        ch <- setSignalHandler (wlrSurfaceEvtCommit events) $ handleOverrideCommit view surf
                        setDestroyHandler (wlrSurfaceEvtDestroy events) (const $ liftIO $ removeListener ch)

                    (Point x y) <- liftIO $ X.getX11SurfacePosition surf
                    (width, height) <- getViewSize view
                    pure . InsertFloating $ WlrBox x y (floor width) (floor height)
                else mempty
