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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Waymonad.Shells.XdgShell
    ( xdgShellCreate
    , XdgShell

    , XdgRef
    , makeShell
    )
where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Monad (filterM, forM_, unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Composition ((.:))
import Data.IORef (newIORef, IORef, modifyIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Server (DisplayServer)
import Graphics.Wayland.Signal (removeListener)
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface, subSurfaceAt, surfaceGetSize)


import Waymonad.Managehook (insertView, removeView)
import Waymonad.Utility.Base (doJust, ptrToInt)
import Waymonad.View
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad.Utility.Log (logPutText, LogPriority (..))
import Waymonad.Utility.Signal (setDestroyHandler, setSignalHandler)
import Waymonad
import Waymonad.Types

import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Graphics.Wayland.WlRoots.XdgShell as R

newtype XdgRef = XdgRef (IORef (Maybe XdgShell))

type MapRef =  IORef (IntMap View)

instance (FocusCore vs ws, WSTag ws) =>  ShellClass XdgRef vs ws where
    {-# SPECIALISE instance FocusCore vs Text => ShellClass XdgRef vs Text #-}
    activateShell (XdgRef ref) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just _ -> pure ()
            Nothing -> do
                dsp <- compDisplay . wayCompositor <$> getState
                shell <- xdgShellCreate dsp
                liftIO $ writeIORef ref $ Just shell
    deactivateShell (XdgRef ref) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just XdgShell {xdgWlrootsShell = roots} -> liftIO $ do
                R.xdgShellDestroy roots
                writeIORef ref Nothing
            Nothing -> pure ()
    isShellActive (XdgRef ref) = do
        ret <- liftIO $ readIORef ref
        pure $ case ret of
            Just _ -> True
            Nothing -> False
    getShellName _ = pure $ "XdgShell (v6)"
    getShellViews (XdgRef ref) = liftIO $ do
        ret <- readIORef ref
        case ret of
            Nothing -> pure mempty
            Just XdgShell {xdgSurfaceRef = surfRef} -> do
                surfMap <- readIORef surfRef
                pure $ S.fromList $ M.elems surfMap

makeShell :: (FocusCore vs ws, WSTag ws) => IO (WayShell vs ws)
makeShell = WayShell . XdgRef <$> liftIO (newIORef Nothing)

data XdgShell = XdgShell
    { xdgSurfaceRef :: {-# UNPACK #-} !MapRef
    , xdgWlrootsShell :: {-# UNPACK #-} !(Ptr R.WlrXdgShell)
    }

newtype XdgSurface = XdgSurface { unXdg :: Ptr R.WlrXdgSurface }

xdgShellCreate
    :: (FocusCore vs a, WSTag a)
    => DisplayServer
    -> Way vs a XdgShell
xdgShellCreate display = do
    surfaces <- liftIO $ newIORef mempty
    roots <- setCallback
        (handleXdgSurface surfaces)
        (`R.xdgShellCreate` display)

    logPutText loggerXdg Trace "Created xdg_shell_v6 handler"

    pure XdgShell
        { xdgSurfaceRef = surfaces
        , xdgWlrootsShell = roots
        }

handleXdgDestroy
    :: (FocusCore vs a, WSTag a)
    => MapRef
    -> Ptr R.WlrXdgSurface
    -> Way vs a ()
handleXdgDestroy ref surf = do
    logPutText loggerXdg Debug "Destroying xdg toplevel surface"
    view <- fromJust . M.lookup (ptrToInt surf) <$> liftIO (readIORef ref)
    liftIO $ modifyIORef ref $ M.delete (ptrToInt surf)

    removeView view
    triggerViewDestroy view

handleXdgPopup :: View -> IO Point -> Ptr R.WlrXdgPopup -> Way vs ws ()
handleXdgPopup view getParentPos pop = do
    base <- liftIO $ R.xdgPopupGetBase pop
    let getPos = do
            Point parentX parentY <- getParentPos
            popBox <- liftIO $ R.getGeometry base
            stateBox <- liftIO $ R.getPopupGeometry base

            let popX = boxX popBox
            let popY = boxY popBox

            let stateX = boxX stateBox
            let stateY = boxY stateBox

            let x = parentX + stateX - popX
            let y = parentY + stateY - popY

            pure $ Point x y

    let signals = R.getXdgSurfaceEvents base
    doJust (liftIO $ R.xdgSurfaceGetSurface base) $ \popSurf -> do
        viewAddSurf view (R.xdgSurfaceEvtDestroy signals) getPos popSurf

    handler <- setSignalHandler (R.xdgSurfaceEvtPopup signals) $ handleXdgPopup view getPos
    setDestroyHandler (R.xdgSurfaceEvtDestroy signals) $ \_ -> do
        liftIO $ removeListener handler


handleXdgSurface
    :: (FocusCore vs a, WSTag a)
    => MapRef
    -> Ptr R.WlrXdgSurface
    -> Way vs a ()
handleXdgSurface ref surf = do
    isPopup <- liftIO $ R.isXdgPopup surf
    unless isPopup $ do
        logPutText loggerXdg Debug "New xdg toplevel surface"
        let xdgSurf = XdgSurface surf
        view <- createView xdgSurf
        insertView view

        liftIO $ do
            modifyIORef ref $ M.insert (ptrToInt surf) view
            R.setMaximized surf True

        let signals = R.getXdgSurfaceEvents surf
        let getPos = do
                WlrBox x y _ _ <- liftIO $ R.getGeometry surf
                pure $ Point x y
        handler <- setSignalHandler (R.xdgSurfaceEvtPopup signals) $ handleXdgPopup view getPos
        setDestroyHandler (R.xdgSurfaceEvtDestroy signals) $ \surfPtr -> do
            liftIO $ removeListener handler
            handleXdgDestroy ref surfPtr

getXdgBox :: MonadIO m => Ptr R.WlrXdgSurface -> m WlrBox
getXdgBox surf = do
    geo@(WlrBox _ _ gw gh) <- liftIO $ R.getGeometry surf
    if gw == 0 || gh == 0
        then do
            ret <- liftIO $ R.xdgSurfaceGetSurface surf
            case ret of
                Nothing -> pure $ WlrBox 0 0 0 0
                Just wlrSurf -> do
                    Point w h <- liftIO $ surfaceGetSize wlrSurf
                    pure $ WlrBox 0 0 w h
        else pure $ geo

renderPopups :: (Ptr WlrSurface -> WlrBox -> IO ()) -> Ptr R.WlrXdgSurface -> IO ()
renderPopups fun surf = do
    popups <- liftIO $ filterM R.isConfigured =<< R.xdgGetPopupSurfaces surf
    surfBox <- liftIO $ R.getGeometry surf
    let surfX = boxX surfBox
    let surfY = boxY surfBox
    forM_ popups $ \popup -> do
        popBox <- getXdgBox popup
        let popX = boxX popBox
        let popY = boxY popBox

        stateBox <- liftIO $ R.getPopupGeometry popup
        let stateX = boxX stateBox
        let stateY = boxY stateBox

        let x = surfX + stateX - popX
        let y = surfY + stateY - popY

        let box = WlrBox x y (boxWidth popBox) (boxHeight popBox)

        doJust (liftIO $ R.xdgSurfaceGetSurface popup) $ \wlrSurf -> do
            fun wlrSurf (traceShowId box)
            renderPopups
                (\v b -> fun v b {boxX = boxX b + x, boxY = boxY b + y})
                popup

xdgPopupAt :: MonadIO m => XdgSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
xdgPopupAt (XdgSurface surf) x y = do
    (popup, popx, popy) <- MaybeT (liftIO $ R.xdgPopupAt surf x y)
    ret <- MaybeT (liftIO $ R.xdgSurfaceGetSurface popup)
    pure $ (ret, x - popx, y - popy)

xdgSubsurfaceAt :: MonadIO m => XdgSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
xdgSubsurfaceAt (XdgSurface surf) x y = do
    wlrsurf <- MaybeT (liftIO $ R.xdgSurfaceGetSurface surf)
    MaybeT (liftIO $ subSurfaceAt wlrsurf x y)

xdgMainSurf :: MonadIO m => XdgSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
xdgMainSurf (XdgSurface surf) x y = MaybeT . liftIO $ do
    WlrBox _ _ w h <- getXdgBox surf
    if x > 0 && x < fromIntegral w && y > 0 && y < fromIntegral h
        then do
            realS <- R.xdgSurfaceGetSurface surf
            pure $ fmap (, x, y) realS
        else pure Nothing

getXdgEventSurface :: MonadIO m => XdgSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
{-# SPECIALISE getXdgEventSurface :: XdgSurface -> Double -> Double -> MaybeT IO (Ptr WlrSurface, Double, Double) #-}
{-# SPECIALISE getXdgEventSurface :: XdgSurface -> Double -> Double -> MaybeT (Way vs ws) (Ptr WlrSurface, Double, Double) #-}
getXdgEventSurface surf x y =
    xdgPopupAt surf x y <|> xdgSubsurfaceAt surf x y <|> xdgMainSurf surf x y


instance ShellSurface XdgSurface where
    close = liftIO . R.sendClose . unXdg
    getSurface = liftIO . R.xdgSurfaceGetSurface . unXdg
    getSize surf = do
        WlrBox _ _ w h <- getXdgBox $ unXdg surf
        pure $ (fromIntegral w, fromIntegral h)
    resize (XdgSurface surf) width height =
        liftIO $ R.setSize surf width height
    activate = liftIO .: R.setActivated . unXdg
    renderAdditional fun (XdgSurface surf) = renderPopups fun surf
    getEventSurface surf x y = runMaybeT (getXdgEventSurface surf x y)
    getID = ptrToInt . unXdg
    getTitle = liftIO . R.getTitle . unXdg
    getAppId = liftIO . R.getAppId . unXdg
    hasCSD _ = pure True
