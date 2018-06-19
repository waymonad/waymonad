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
module Waymonad.Shells.WlShell
    ( WlRef
    , makeShell
    )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.IORef (newIORef, IORef, modifyIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Foreign.Ptr (Ptr)
import Data.Text (Text)

import Graphics.Wayland.Server (DisplayServer, clientDestroy)
import Graphics.Wayland.Signal (ListenerToken, removeListener)
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface, surfaceAt, surfaceGetSize)


import Waymonad.Managehook (insertView, removeView)
import Waymonad.Utility.Base (doJust, ptrToInt)
import Waymonad.View
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad.Utility.Signal (setDestroyHandler, setSignalHandler)
import Waymonad
import Waymonad.Types

import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Graphics.Wayland.WlRoots.WlShell as R

newtype WlRef = WlRef (IORef (Maybe WlShell))

type MapRef =  IORef (IntMap View)

instance (FocusCore vs ws, WSTag ws) =>  ShellClass WlRef vs ws where
    {-# SPECIALIZE instance (FocusCore vs Text) => ShellClass WlRef vs Text #-}
    activateShell (WlRef ref) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just _ -> pure ()
            Nothing -> do
                dsp <- compDisplay . wayCompositor <$> getState
                shell <- wlShellCreate dsp
                liftIO $ writeIORef ref $ Just shell
    deactivateShell (WlRef ref) = do
        ret <- liftIO $ readIORef ref
        case ret of
            Just WlShell {wlWlrootsShell = roots, wlShellToken = tok} -> liftIO $ do
                removeListener tok
                R.shellDestroy roots
                writeIORef ref Nothing
            Nothing -> pure ()
    isShellActive (WlRef ref) = do
        ret <- liftIO $ readIORef ref
        pure $ case ret of
            Just _ -> True
            Nothing -> False
    getShellName _ = pure $ "WlShell"
    getShellViews (WlRef ref) = liftIO $ do
        ret <- readIORef ref
        case ret of
            Nothing -> pure mempty
            Just WlShell {wlSurfaceRef = surfRef} -> do
                surfMap <- readIORef surfRef
                pure $ S.fromList $ M.elems surfMap

makeShell :: (FocusCore vs ws, WSTag ws) => IO (WayShell vs ws)
makeShell = WayShell . WlRef <$> liftIO (newIORef Nothing)

data WlShell = WlShell
    { wlSurfaceRef   :: {-# UNPACK #-} !MapRef
    , wlWlrootsShell :: {-# UNPACK #-} !R.WlrWlShell
    , wlShellToken   :: !ListenerToken
    }

newtype WlSurface = WlSurface { unWl :: R.WlrWlShellSurface }

wlShellCreate :: (FocusCore vs a, WSTag a)
              => DisplayServer
              -> Way vs a WlShell
wlShellCreate display = do
    surfaces <- liftIO $ newIORef mempty
    roots <- liftIO $ R.shellCreate display
    token <- setCallback (handleWlSurface surfaces) (R.setWlShellListener roots)

    pure WlShell
        { wlSurfaceRef = surfaces
        , wlWlrootsShell = roots
        , wlShellToken = token
        }

handleWlDestroy :: (FocusCore vs a, WSTag a)
                => MapRef
                -> Ptr R.WlrWlShellSurface
                -> Way vs a ()
handleWlDestroy ref surf = do
    view <- fromJust . M.lookup (ptrToInt surf) <$> liftIO (readIORef ref)
    liftIO $ modifyIORef ref $ M.delete (ptrToInt surf)

    removeView view
    triggerViewDestroy view


handleWlPopup :: View -> IO Point -> Ptr R.WlrWlShellSurface -> Way vs ws ()
handleWlPopup view getParentPos ptr = do
    let popup = R.WlrWlShellSurface ptr
    let getPos = do
            transient <- liftIO $ R.getTransientPosition popup
            case transient of
                Just (x, y) -> do
                    Point parentX parentY <- getParentPos
                    pure $ Point (parentX + fromIntegral x) (parentY + fromIntegral y)
                Nothing -> pure $ Point 0 0

    let signals = R.getWlrWlSurfaceEvents popup
    doJust  (liftIO $ R.wlShellSurfaceGetSurface popup) $
        viewAddSurf view (R.wlrWlSurfaceEvtDestroy signals) getPos

    handler <- setSignalHandler (R.wlrWlSurfaceEvtPopup signals) $ handleWlPopup view getPos
    setDestroyHandler (R.wlrWlSurfaceEvtDestroy signals) $ \_ -> do
        liftIO $ removeListener handler

handleWlSurface :: (FocusCore vs a, WSTag a)
                => MapRef
                -> R.WlrWlShellSurface
                -> Way vs a ()
handleWlSurface ref surf = do
    isPopup <- liftIO $ R.isPopup surf
    unless isPopup $ do
        let wlSurf = WlSurface surf
        view <- createView wlSurf
        insertView view

        liftIO $ do
            modifyIORef ref $ M.insert (ptrToInt $ R.unWlrSurf surf) view

        let signals = R.getWlrWlSurfaceEvents surf
        handler <- setSignalHandler (R.wlrWlSurfaceEvtPopup signals) $ handleWlPopup view $ pure (Point 0 0 )
        setDestroyHandler (R.wlrWlSurfaceEvtDestroy signals) (\arg -> do
            liftIO $ removeListener handler
            handleWlDestroy ref arg
                                                             )


renderPopups :: (Ptr WlrSurface -> WlrBox -> IO ()) -> R.WlrWlShellSurface -> IO ()
renderPopups fun surf = do
    popups <- liftIO $ R.getWlShellPopups surf
    forM_ popups $ \popup -> doJust (liftIO $ R.getTransientPosition popup) $ \(popX, popY) -> do
        doJust (liftIO $ R.wlShellSurfaceGetSurface popup) $ \wlrSurf -> do
            Point w h <- liftIO $ surfaceGetSize wlrSurf
            let x = fromIntegral popX
            let y = fromIntegral popY
            let box = WlrBox x y w h
            fun wlrSurf box
            renderPopups
                (\v b -> fun v b {boxX = boxX b + x, boxY = boxY b + y})
                popup

getBoundingBox :: R.WlrWlShellSurface -> IO (Double, Double)
getBoundingBox surf = doJust (R.wlShellSurfaceGetSurface surf) $ \wlrsurf -> do
    Point bw bh <-  surfaceGetSize wlrsurf
    pure ((fromIntegral bw), (fromIntegral bh))

wlPopupAt :: MonadIO m => WlSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
wlPopupAt (WlSurface surf) x y = do
    (ret, popx, popy) <- MaybeT (liftIO $ R.shellSurfaceAt surf x y)
    pure $ (ret, popx, popy)

wlSubsurfaceAt :: MonadIO m => WlSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
wlSubsurfaceAt (WlSurface surf) x y = do
    wlrsurf <- MaybeT (liftIO $ R.wlShellSurfaceGetSurface surf)
    MaybeT (liftIO $ surfaceAt wlrsurf x y)

wlMainSurf :: MonadIO m => WlSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
wlMainSurf (WlSurface surf) x y = MaybeT . liftIO $ do
    (w, h) <- getBoundingBox surf
    if x > 0 && x < w && y > 0 && y < h
        then do
            realS <- R.wlShellSurfaceGetSurface surf
            pure $ fmap (, x, y) realS
        else pure Nothing

getWlEventSurface :: MonadIO m => WlSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
{-# SPECIALIZE getWlEventSurface :: WlSurface -> Double -> Double -> MaybeT IO (Ptr WlrSurface, Double, Double) #-}
getWlEventSurface surf x y =
    wlPopupAt surf x y <|> wlSubsurfaceAt surf x y <|> wlMainSurf surf x y


instance ShellSurface WlSurface where
    close surf = liftIO $ do
        client <- R.getClient $ unWl surf
        clientDestroy client
    getSurface = liftIO . R.wlShellSurfaceGetSurface . unWl
    getSize = liftIO . getBoundingBox . unWl
    resize (WlSurface surf) width height _ = do
        liftIO $ R.configureWlShellSurface surf (fromIntegral width) (fromIntegral height)
        pure False
    activate _ _ = pure ()
    renderAdditional fun (WlSurface surf) = renderPopups fun surf
    getEventSurface surf x y = runMaybeT (getWlEventSurface surf x y)
    getID = ptrToInt . R.unWlrSurf . unWl
    getTitle = liftIO . R.getTitle . unWl
    getAppId = liftIO . R.getClass . unWl
    hasCSD _ = pure True
