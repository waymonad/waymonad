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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Waymonad.Shells.XWayland
    ( xwayShellCreate
    , XWayShell
    , overrideXRedirect

    , XWayRef
    , makeShell
    , makeShellAct
    , xwayGetPid
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask, when)
import Data.IORef (newIORef, IORef, readIORef, writeIORef, modifyIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
    ( castPtrToStablePtr, castStablePtrToPtr, freeStablePtr, newStablePtr)
import Foreign.Storable (Storable(..))
import System.IO
import System.Posix.Types

import Graphics.Pixman (withBoxRegion)

import Graphics.Wayland.Server (DisplayServer)
import Graphics.Wayland.Signal (removeListener, ListenerToken)
import Graphics.Wayland.WlRoots.Box (Point(..), WlrBox(..), boxContainsPoint)
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.Surface
    (WlrSurface, WlrSurfaceEvents (..)
    , getWlrSurfaceEvents
    )
import Waymonad.Input.Seat
import Waymonad.Managehook
import Waymonad.Utility.Base
import Waymonad.View
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad.Utility.Floating (resizeFloat, moveFloat)
import Waymonad.Utility.Log (logPutText, LogPriority (..))
import Waymonad.Utility.Signal (setSignalHandler, setDestroyHandler)
import Waymonad
import Waymonad.Types
    ( Compositor (..),  ShellClass (..), WayBindingState (..), WayShell (..)
    )
import Waymonad.Types.Core (SeatEvent (SeatKeyboard))

import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Graphics.Wayland.WlRoots.XWayland as X

data XWayRef vs ws = XWayRef
    (IORef (Maybe XWayShell))
    (Way vs ws ())

attachSeat :: Ptr X.XWayland -> Way vs ws ()
attachSeat roots = doJust getSeat $ \seat -> liftIO $ do
    hPutStrLn stderr "Setting the X seat"
    liftIO $ X.setXWaylandSeat roots (seatRoots seat)

makeShellAct :: (Typeable vs, Typeable ws, FocusCore vs ws, WSTag ws)
             => Way vs ws () -> IO (WayShell vs ws)
makeShellAct act = do
    ref <- newIORef Nothing
    pure $ WayShell $ XWayRef ref act

makeShell :: (Typeable vs, Typeable ws, FocusCore vs ws, WSTag ws)
          => IO (WayShell vs ws)
makeShell = makeShellAct (pure ())

instance (Typeable vs, Typeable ws, FocusCore vs ws, WSTag ws) => ShellClass (XWayRef vs ws) vs ws where
    {-# SPECIALIZE instance (FocusCore vs Text, Typeable vs) => ShellClass (XWayRef vs Text) vs Text #-}
    activateShell (XWayRef ref act) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just _ -> pure ()
            Nothing -> do
                dsp <- compDisplay . wayCompositor <$> getState
                comp <- compCompositor . wayCompositor <$> getState
                shell <- xwayShellCreate dsp comp act
                liftIO $ writeIORef ref $ Just shell
    deactivateShell (XWayRef ref _) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just XWayShell {xwayWlrootsShell = roots} -> liftIO $ do
                X.xwaylandDestroy roots
                writeIORef ref Nothing
            Nothing -> pure ()
    isShellActive (XWayRef ref _) = do
        ret <- liftIO $ readIORef ref
        pure $ case ret of
            Just _ -> True
            Nothing -> False
    getShellName _ = pure "XWayland"
    getShellViews (XWayRef ref _) = liftIO $ do
        ret <- readIORef ref
        case ret of
            Nothing -> pure mempty
            Just XWayShell {xwaySurfaceRef = surfRef} -> do
                surfMap <- readIORef surfRef
                pure $ S.fromList $ M.elems surfMap

type MapRef =  IORef (IntMap View)

data XWayShell = XWayShell
    { xwayWlrootsShell :: {-# UNPACK #-} !(Ptr X.XWayland)
    , xwaySurfaceRef   :: {-# UNPACK #-} !MapRef
    }

newtype XWaySurface = XWaySurface { unXway :: Ptr X.X11Surface }

xwayGetPid :: MonadIO m => XWaySurface -> m (Maybe ProcessID)
{-# SPECIALIZE xwayGetPid :: XWaySurface -> Way vs ws (Maybe ProcessID) #-}
xwayGetPid XWaySurface { unXway = roots } = liftIO $ do
    pid <- X.getX11Pid roots
    pure $ if pid == 0
        then Nothing
        else Just pid


xwayShellCreate :: (FocusCore vs a, WSTag a)
                => DisplayServer
                -> Ptr WlrCompositor
                -> Way vs a ()
                -> Way vs a XWayShell
xwayShellCreate display comp act = do
    surfaces <- liftIO $ newIORef mempty
    roots <- liftIO $ X.xwaylandCreate display comp False

    setCallback (handleXwaySurface surfaces) (X.xwayBindNew roots)
    setDestroyHandler (X.xwayReadEvent roots) (pure (attachSeat roots >> act))

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

handleX11Configure :: (WSTag ws, FocusCore vs ws) => View -> Ptr X.X11Surface -> Ptr X.ConfigureEvent -> Way vs ws ()
handleX11Configure view surf evt = do
    logPutText loggerX11 Debug "Got configure request"
    override <- liftIO $ X.x11SurfaceOverrideRedirect surf
    when override $ do
        event <- liftIO $ peek evt
        let width  = fromIntegral $ X.configureEvtWidth event
        let height = fromIntegral $ X.configureEvtHeight event
        let x = fromIntegral $ X.configureEvtX event
        let y = fromIntegral $ X.configureEvtY event
        moveView view x y
        updateViewSize view width height
        moveFloat view x y
        resizeFloat view width height

handleX11Map :: (FocusCore vs ws, WSTag ws)
             => View -> MapRef -> Ptr X.X11Surface -> Way vs ws ()
handleX11Map view ref surf = do
    WlrBox _ _ w h <- liftIO $ X.getX11SurfaceGeometry surf
    doJust (liftIO $ X.xwaySurfaceGetSurface surf) $ \mainSurf -> liftIO $ do
        sizeRef <- newIORef (fromIntegral w, fromIntegral h)
        setViewMainSurface
            view
            sizeRef
            mainSurf

    override <- liftIO $ X.x11SurfaceOverrideRedirect surf
    parent <- if override
        then liftIO $ do
            doJust (X.getX11ParentSurfrace surf) $ \parent ->
                fmap (const parent) <$> X.xwaySurfaceGetSurface parent
        else pure Nothing
    case parent of
        Nothing -> do
            logPutText loggerX11 Trace "Creating an normal view"
            Point x y <- liftIO $ X.getX11SurfacePosition surf
            moveView view x y
            insertView view
        Just parentSurf -> do
            logPutText loggerX11 Trace "Surface has (real) parent. Skipping view creation"
            doJust (liftIO (getAncestorView parentSurf =<< readIORef ref)) $ \(viewSurf, pView) -> do
                logPutText loggerX11 Trace $ "Attaching to view: " `T.append` T.pack (show pView)
                handleAddChild pView viewSurf surf

handleChildMap :: Bool -> View -> Ptr X.X11Surface -> Ptr X.X11Surface -> Way vs ws ()
handleChildMap addSurf view viewSurf surf = liftIO $ if addSurf
    then doJust (liftIO $ X.xwaySurfaceGetSurface surf) $ \mainSurf -> do
        viewAddSurf
            view
            (wlrSurfaceEvtDestroy $ getWlrSurfaceEvents mainSurf)
            (do
                WlrBox x y _ _ <- X.getX11SurfaceGeometry viewSurf
                WlrBox cx cy _ _ <- X.getX11SurfaceGeometry surf
                pure $ Point (cx - x) (cy - y)
            )
            mainSurf
    else do
        WlrBox x y _ _ <- X.getX11SurfaceGeometry viewSurf
        WlrBox cx cy w h <- X.getX11SurfaceGeometry surf
        withBoxRegion (WlrBox (cx - x) (cy - y) w h) $ doApplyDamage view


handleAddChild :: View -> Ptr X.X11Surface -> Ptr X.X11Surface -> Way vs ws ()
handleAddChild view viewSurf surf = do
    let signals = X.getX11SurfaceEvents surf
    handleChildMap True view viewSurf surf

    unMapH <- setSignalHandler (X.x11SurfaceEvtUnmap signals) $ handleChildMap False view viewSurf
    mapH <- setSignalHandler (X.x11SurfaceEvtMap signals) $ handleChildMap True view viewSurf

    setDestroyHandler (X.x11SurfaceEvtDestroy signals) (liftIO . const (removeListener unMapH >> removeListener mapH))

getAncestorView :: Ptr X.X11Surface -> IntMap View -> IO (Maybe (Ptr X.X11Surface, View))
getAncestorView surf surfMap = case M.lookup (ptrToInt surf) surfMap of
    Just x -> pure $ Just (surf, x)
    Nothing -> doJust (X.getX11ParentSurfrace surf) $ \parent ->
        getAncestorView parent surfMap

handleXwaySurface
    :: (FocusCore vs a, WSTag a)
    => MapRef
    -> Ptr X.X11Surface
    -> Way vs a ()
handleXwaySurface ref surf = do
    let xwaySurf = XWaySurface surf
    logPutText loggerX11 Debug "New XWayland surface"

    view <- createView xwaySurf
    liftIO $ modifyIORef ref $ M.insert (ptrToInt surf) view

    let signals = X.getX11SurfaceEvents surf

    --h1 <- setSignalHandler (X.x11SurfaceEvtType signals) $ const $ liftIO $ hPutStrLn stderr "Some surface set type"
    --h2 <- setSignalHandler (X.x11SurfaceEvtParent signals) $ const . liftIO $ (hPutStrLn stderr "Something set the parent")
    --h3 <- setSignalHandler (X.x11SurfaceEvtMove signals) $ const . liftIO $ (hPutStrLn stderr "Something requests a move")
    h4 <- setSignalHandler (X.x11SurfaceEvtConfigure signals) $ handleX11Configure view surf
    h5 <- setSignalHandler (X.x11SurfaceEvtUnmap signals) $ const (removeView view)
    h6 <- setSignalHandler (X.x11SurfaceEvtMap signals) $ handleX11Map view ref

    setDestroyHandler (X.x11SurfaceEvtDestroy signals) $ handleXwayDestroy ref [{-h1, h2, h3, -}h4, h5, h6]
    liftIO $ do
        stPtr <- newStablePtr view
        poke (X.getX11SurfaceDataPtr surf) (castStablePtrToPtr stPtr)


instance ShellSurface XWaySurface where
    close (XWaySurface surf) = liftIO $ X.xwayCloseSurface surf
    getSurface = liftIO . X.xwaySurfaceGetSurface . unXway
    getSize (XWaySurface surf) = liftIO $ do
        box <- X.getX11SurfaceGeometry surf
        pure (fromIntegral $ boxWidth box, fromIntegral $ boxHeight box)
    resize (XWaySurface surf) width height _ = liftIO $ do
        (Point x y) <- X.getX11SurfacePosition surf
        X.configureX11Surface surf
            (fromIntegral x) (fromIntegral y)
            (fromIntegral width) (fromIntegral height)
        pure False
    activate (XWaySurface surf) active = liftIO $ X.activateX11Surface surf active
    getEventSurface (XWaySurface surf) x y = liftIO $ do
        (WlrBox _ _ w h) <- X.getX11SurfaceGeometry surf
        if boxContainsPoint (Point (floor x) (floor y)) (WlrBox 0 0 w h)
           then do
                ret <- X.xwaySurfaceGetSurface surf
                pure $ fmap (,x, y) ret
            else pure Nothing
    setPosition (XWaySurface surf) x y =
        let point = Point x y
         in liftIO $ X.setX11SurfacePosition surf point
    getID (XWaySurface surf) = ptrToInt surf
    getTitle = liftIO . X.getTitle . unXway
    getAppId = liftIO . X.getClass . unXway
    renderAdditional fun (XWaySurface surf) = renderChildren fun surf
    hasCSD _ = pure False
    takesFocus (XWaySurface surf) SeatKeyboard = liftIO $ do
        oR <- X.x11SurfaceOverrideRedirect surf
        if oR
            then X.x11ORWantsFocus surf
            else pure False
    takesFocus _ _ = pure True

renderChildren :: (Ptr WlrSurface -> WlrBox -> IO ()) -> Ptr X.X11Surface -> IO ()
renderChildren fun surf = do
    children <- liftIO $ X.getX11Children surf
    WlrBox sx sy _ _  <- liftIO $ X.getX11SurfaceGeometry surf

    forM_ children $ \child -> do
        override <- liftIO $ X.x11SurfaceOverrideRedirect child
        mapped <- liftIO $ X.isX11Mapped child
        when (override && mapped) $ do
            WlrBox cx cy cw ch <- liftIO $ X.getX11SurfaceGeometry child
            doJust (liftIO $ X.xwaySurfaceGetSurface child) $ \wlrSurf ->
                fun wlrSurf (WlrBox (cx - sx) (cy - sy) cw ch)
            renderChildren fun child

overrideXRedirect :: (WSTag ws, FocusCore vs ws) => Managehook vs ws
overrideXRedirect = do
    view <- ask
    getViewInner view >>= \case
        Nothing -> mempty
        Just (XWaySurface surf) -> do
            override <- liftIO $ X.x11SurfaceOverrideRedirect surf
            if override
                then do
                    liftWay $ logPutText loggerX11 Info "Overriding a redirect."

                    (Point x y) <- liftIO $ X.getX11SurfacePosition surf
                    (width, height) <- getViewSize view
                    pure . InsertFloating $ WlrBox x y (floor width) (floor height)
                else mempty
