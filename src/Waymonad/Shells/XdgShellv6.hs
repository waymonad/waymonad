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
module Waymonad.Shells.XdgShellv6
    ( xdgShellCreate
    , XdgShell

    , XdgRef
    , makeShell
    )
where

import Control.Applicative ((<|>))
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Composition ((.:))
import Data.IORef (newIORef, IORef, modifyIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.Server (DisplayServer, callbackDone)
import Graphics.Wayland.Signal (removeListener)
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), translateBox)
import Graphics.Wayland.WlRoots.Output (getEffectiveBox)
import Graphics.Wayland.WlRoots.Surface
    (WlrSurface, surfaceAt, surfaceGetSize
    , getWlrSurfaceEvents, WlrSurfaceEvents(..)
    , subSurfaceGetSurface, surfaceGetSubs
    , getCurrentState, surfaceGetCallbacks, callbackGetCallback, callbackGetResource
    )


import Waymonad.Managehook (insertView, configureView, removeView)
import Waymonad.Utility.Base (doJust, ptrToInt)
import Waymonad.View
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad.Utility.Log (logPutText, LogPriority (..))
import Waymonad.Utility.Signal (setDestroyHandler, setSignalHandler)
import Waymonad
import Waymonad.Types

import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Graphics.Wayland.WlRoots.XdgShellv6 as R

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

data XdgSurface = XdgSurface
    { unXdg         :: Ptr R.WlrXdgSurface
    , xdgSurfConfig :: IORef Word32
    , xdgSurfAck    :: IORef (IO ())
    }

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

handleXdgDestroy :: (FocusCore vs a, WSTag a)
                 => MapRef -> Ptr R.WlrXdgSurface -> Way vs a ()
handleXdgDestroy ref surf = do
    logPutText loggerXdg Debug "Destroying xdg toplevel surface"
    view <- fromJust . M.lookup (ptrToInt surf) <$> liftIO (readIORef ref)
    liftIO $ modifyIORef ref $ M.delete (ptrToInt surf)
    triggerViewDestroy view

unconstrainPopup :: View -> Ptr R.WlrXdgPopup -> Way vs ws ()
unconstrainPopup view popup = do
    outs <- doGetPosition view
    case outs of
        [(out, Point x y)] -> liftIO $ do
            WlrBox _ _ w h <- getEffectiveBox out
            R.unconstrainPopup popup $ WlrBox (negate x) (negate y) w h
        _ -> pure ()

handleXdgPopup :: View -> IO Point -> Ptr R.WlrXdgPopup -> Way vs ws ()
handleXdgPopup view getParentPos pop = do
    unconstrainPopup view pop
    base <- liftIO $ R.xdgPopupGetBase pop

    pos <- liftIO $ do
            Point parentX parentY <- getParentPos
            stateBox <- liftIO $ fmap (fromMaybe (WlrBox 0 0 0 0)) $ R.getPopupGeometry base

            let stateX = boxX stateBox
            let stateY = boxY stateBox

            let x = parentX + stateX
            let y = parentY + stateY

            pure $ Point x y

    let getPopPos = pure pos
    let getSurfPos = do
            Point x y <- getPopPos
            popBox <- liftIO $ R.getGeometry base

            let popX = boxX popBox
            let popY = boxY popBox
            pure $ Point (x - popX) (y - popY)

    let signals = R.getXdgSurfaceEvents base
    doJust (liftIO $ R.xdgSurfaceGetSurface base) $ \popSurf -> do
        viewAddSurf view (R.xdgSurfaceEvtDestroy signals) getSurfPos popSurf

    handler <- setSignalHandler (R.xdgSurfaceEvtPopup signals) $ handleXdgPopup view getPopPos
    setDestroyHandler (R.xdgSurfaceEvtDestroy signals) $ \_ -> do
        liftIO $ removeListener handler

handleXdgUnmap :: (FocusCore vs ws, WSTag ws)
               => View -> Ptr R.WlrXdgSurface -> Way vs ws ()
handleXdgUnmap view _ = removeView view

handleXdgMap :: (FocusCore vs ws, WSTag ws)
             => View -> Ptr R.WlrXdgSurface -> Way vs ws ()
handleXdgMap view _ = insertView view

handleXdgCommit :: View -> Ptr R.WlrXdgSurface -> Way vs ws ()
handleXdgCommit view surf = setViewGeometry view =<< liftIO (R.getGeometry surf)

addWlrSurface :: View -> Ptr R.WlrXdgSurface -> Ptr WlrSurface -> Way vs ws ()
addWlrSurface view surf wlrSurf = do
    let wlrEvents = getWlrSurfaceEvents wlrSurf
    commitH <- setSignalHandler (wlrSurfaceEvtCommit wlrEvents) $ const (handleXdgCommit view surf)
    subSurfH <- setSignalHandler (wlrSurfaceEvtSubSurf wlrEvents) $ (\s -> addWlrSurface view surf =<< liftIO (subSurfaceGetSurface s))

    setDestroyHandler (wlrSurfaceEvtDestroy wlrEvents) $ (const . liftIO $ do
        removeListener commitH
        removeListener subSurfH
                                                         )
    handleXdgCommit view surf

handleXdgMainCommit :: XdgSurface -> Way vs ws ()
handleXdgMainCommit XdgSurface {unXdg = surf, xdgSurfAck = ackRef, xdgSurfConfig = serialRef} = liftIO $ do
    expected <- readIORef serialRef
    when (expected > 0) $ do
        serial <- R.getConfigureSerial surf
        when (serial >= expected) $ do
            writeIORef serialRef 0
            ack <- readIORef ackRef
            writeIORef ackRef $ pure ()
            ack

handleXdgSurface :: (FocusCore vs a, WSTag a)
                 => MapRef -> Ptr R.WlrXdgSurface -> Way vs a ()
handleXdgSurface ref surf = do
    isPopup <- liftIO $ R.isXdgPopup surf
    unless isPopup $ do
        logPutText loggerXdg Debug "New xdg toplevel surface"
        serialRef <- liftIO $ newIORef 0
        ackRef <- liftIO $ newIORef (pure ())
        let xdgSurf = XdgSurface surf serialRef ackRef
        view <- createView xdgSurf

        liftIO $ do
            modifyIORef ref $ M.insert (ptrToInt surf) view
            R.setMaximized surf True

        let signals = R.getXdgSurfaceEvents surf
        let getPos = do
                WlrBox x y _ _ <- liftIO $ R.getGeometry surf
                pure $ Point x y
        popupH <- setSignalHandler (R.xdgSurfaceEvtPopup signals) $ handleXdgPopup view getPos
        mapH <- setSignalHandler (R.xdgSurfaceEvtMap signals) $ handleXdgMap view
        unmapH <- setSignalHandler (R.xdgSurfaceEvtUnmap signals) $ handleXdgUnmap view

        wlrSurfM <- liftIO $ R.xdgSurfaceGetSurface surf
        mainH <- case wlrSurfM of
            Nothing -> pure []
            Just wlrSurf -> do
                subs <- liftIO $ surfaceGetSubs wlrSurf
                forM_ subs $ \sub -> do
                    addWlrSurface view surf =<< liftIO (subSurfaceGetSurface sub)
                addWlrSurface view surf wlrSurf

                let wlrEvents = getWlrSurfaceEvents wlrSurf
                mainH <- setSignalHandler (wlrSurfaceEvtCommit wlrEvents) $ const $ handleXdgMainCommit xdgSurf
                pure [mainH]

        setDestroyHandler (R.xdgSurfaceEvtDestroy signals) $ \surfPtr -> do
            liftIO $ mapM_ removeListener $ [popupH, mapH, unmapH] ++ mainH
            handleXdgDestroy ref surfPtr

        configureView view

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
    forM_ popups $ \popup -> do
        popBox <- liftIO $ R.getGeometry popup
        let popX = boxX  popBox
        let popY = boxY popBox

        stateBox <- liftIO $ fmap (fromMaybe (WlrBox 0 0 0 0)) $ R.getPopupGeometry popup
        let stateX = boxX stateBox
        let stateY = boxY stateBox

        let x = stateX - popX
        let y = stateY - popY

        let box = WlrBox x y (boxWidth popBox) (boxHeight popBox)

        doJust (liftIO $ R.xdgSurfaceGetSurface popup) $ \wlrSurf -> do
            Point w h <- liftIO $ surfaceGetSize wlrSurf
            fun wlrSurf box { boxWidth = w, boxHeight = h }
            renderPopups
                (\s b -> fun s $ translateBox stateX stateY b)
                popup

xdgPopupAt :: MonadIO m => XdgSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
xdgPopupAt XdgSurface {unXdg = surf} x y = do
    (ret, popx, popy) <- MaybeT (liftIO $ R.xdgSurfaceAt surf x y)
    pure $ (ret, popx, popy)

xdgSubsurfaceAt :: MonadIO m => XdgSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
xdgSubsurfaceAt XdgSurface {unXdg = surf} x y = do
    wlrsurf <- MaybeT (liftIO $ R.xdgSurfaceGetSurface surf)
    MaybeT (liftIO $ surfaceAt wlrsurf x y)

xdgMainSurf :: MonadIO m => XdgSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
xdgMainSurf XdgSurface {unXdg = surf} x y = MaybeT . liftIO $ do
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
    resize XdgSurface {unXdg = surf, xdgSurfAck = ackRef, xdgSurfConfig = serialRef} width height ack = liftIO $ do
        WlrBox _ _ gw gh <- R.getGeometry surf
        if fromIntegral gw == width && fromIntegral gh == height
            then pure False
            else do
                writeIORef ackRef ack
                writeIORef serialRef =<< R.setSize surf width height
                --- Testing something :)
                doJust (liftIO $ R.xdgSurfaceGetSurface surf) $ \surface -> do
                    callbacks <- surfaceGetCallbacks $ getCurrentState surface
                    forM_ callbacks $ \callback -> do
                        cb <- callbackGetCallback callback
                        callbackDone cb (0)
                        res <- callbackGetResource callback
                        resourceDestroy res

--                subs <- surfaceGetSubs surface
--                forM_ subs $ \sub -> notifySurface secs =<< subSurfaceGetSurface sub

                pure True
    activate = liftIO .: R.setActivated . unXdg
    renderAdditional fun XdgSurface {unXdg = surf} = renderPopups fun surf
    getEventSurface surf x y = runMaybeT (getXdgEventSurface surf x y)
    getID = ptrToInt . unXdg
    getTitle XdgSurface {unXdg = surf} = liftIO $ do
        toplevel <- R.getXdgToplevel surf
        case toplevel of
            Nothing -> pure Nothing
            Just top ->  R.getTitle top
    getAppId XdgSurface {unXdg = surf} = liftIO $ do
        toplevel <- R.getXdgToplevel surf
        case toplevel of
            Nothing -> pure Nothing
            Just top ->  R.getAppId top
    hasCSD _ = pure True
