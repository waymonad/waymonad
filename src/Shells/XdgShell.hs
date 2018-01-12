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
module Shells.XdgShell
    ( xdgShellCreate
    , XdgShell

    , XdgRef
    , makeShell
    )
where


import Control.Applicative ((<|>))
import Control.Monad (filterM, forM_, unless)
import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Composition ((.:))
import Data.IORef (newIORef, IORef, modifyIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Foreign.Ptr (Ptr, ptrToIntPtr)
import Foreign.StablePtr
    ( castPtrToStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , newStablePtr
    )
import Foreign.Storable (Storable(..))

import Graphics.Wayland.Server (DisplayServer)
import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), boxContainsPoint)
import Graphics.Wayland.WlRoots.Surface (WlrSurface, surfaceGetSubs, subSurfaceGetBox, subSurfaceAt)


import Managehook (insertView, removeView)
import Utility (doJust)
import View
import ViewSet (WSTag, FocusCore)
import WayUtil.Log (logPutText, LogPriority (..))
import WayUtil.Signal (setSignalHandler)
import Waymonad
import Waymonad.Types

import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Graphics.Wayland.WlRoots.XdgShell as R

newtype XdgRef = XdgRef (IORef (Maybe XdgShell))

type MapRef =  IORef (IntMap View)

instance ShellClass XdgRef where
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
    getShellName _ = "XdgShell (v6)"
    getShellViews (XdgRef ref) = liftIO $ do
        ret <- readIORef ref
        case ret of
            Nothing -> pure mempty
            Just XdgShell {xdgSurfaceRef = surfRef} -> do
                surfMap <- readIORef surfRef
                pure $ S.fromList $ M.elems surfMap

makeShell :: IO WayShell
makeShell = WayShell . XdgRef <$> liftIO (newIORef Nothing)

data XdgShell = XdgShell
    { xdgSurfaceRef :: MapRef
    , xdgWlrootsShell :: Ptr R.WlrXdgShell
    }

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

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

    liftIO $ do
        sptr :: Ptr () <- peek (R.getXdgSurfaceDataPtr surf)
        freeStablePtr $ castPtrToStablePtr sptr

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
            activate xdgSurf True
            R.setMaximized surf True

        let signals = R.getXdgSurfaceEvents surf
        destroyHandler <- setSignalHandler (R.xdgSurfaceEvtDestroy signals) (handleXdgDestroy ref)
        liftIO $ do
            sptr <- newStablePtr destroyHandler
            poke (R.getXdgSurfaceDataPtr surf) (castStablePtrToPtr sptr)


renderPopups :: MonadIO m => (Ptr WlrSurface -> WlrBox -> m ()) -> Ptr R.WlrXdgSurface -> m ()
renderPopups fun surf = do
    popups <- liftIO $ filterM R.isConfigured =<< R.getPopups surf
    surfBox <- liftIO $ R.getGeometry surf
    let surfX = boxX surfBox
    let surfY = boxY surfBox
    forM_ popups $ \popup -> do
        popBox <- liftIO $ R.getGeometry popup
        let popX = boxX popBox
        let popY = boxY popBox

        stateBox <- liftIO $ R.getPopupGeometry popup
        let stateX = boxX stateBox
        let stateY = boxY stateBox

        let x = surfX + stateX - popX
        let y = surfY + stateY - popY

        let box = WlrBox x y (boxWidth popBox) (boxHeight popBox)

        doJust (liftIO $ R.xdgSurfaceGetSurface popup) $ \wlrSurf -> do
            fun wlrSurf box
            renderPopups
                (\v b -> fun v b {boxX = boxX b + x, boxY = boxY b + y})
                popup

getBoundingBox :: Ptr R.WlrXdgSurface -> IO (Double, Double)
getBoundingBox surf = doJust (R.xdgSurfaceGetSurface surf) $ \wlrsurf -> do
    WlrBox _ _ bw bh <- R.getGeometry surf
    subs <- surfaceGetSubs wlrsurf
    points <- forM subs $ \sub -> do
        WlrBox x y w h <- subSurfaceGetBox sub
        pure $ ((Point x y), (Point (x + w) (y + h)))
    let topleft = map fst points
        botright = map snd points
        Point lx ly = foldr (\(Point x1 y1) (Point x2 y2) -> Point (min x1 x2) (min y1 y2)) (Point 0 0) topleft
        Point hx hy = foldr (\(Point x1 y1) (Point x2 y2) -> Point (max x1 x2) (max y1 y2)) (Point bw bh) botright
    pure $ (fromIntegral (hx - lx), fromIntegral (hy - ly))

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
    box <- R.getGeometry surf
    if boxContainsPoint (Point (floor x) (floor y)) box
        then do
            realS <- R.xdgSurfaceGetSurface surf
            pure $ fmap (, x, y) realS
        else pure Nothing

getXdgEventSurface :: MonadIO m => XdgSurface -> Double -> Double -> MaybeT m (Ptr WlrSurface, Double, Double)
getXdgEventSurface surf x y =
    xdgPopupAt surf x y <|> xdgSubsurfaceAt surf x y <|> xdgMainSurf surf x y


instance ShellSurface XdgSurface where
    close = liftIO . R.sendClose . unXdg
    getSurface = liftIO . R.xdgSurfaceGetSurface . unXdg
    getSize = liftIO . getBoundingBox . unXdg
    resize (XdgSurface surf) width height =
        liftIO $ R.setSize surf width height
    activate = liftIO .: R.setActivated . unXdg
    renderAdditional fun (XdgSurface surf) = renderPopups fun surf
    getEventSurface surf x y = runMaybeT (getXdgEventSurface surf x y)
    getID = ptrToInt . unXdg
    getTitle = liftIO . R.getTitle . unXdg
    getAppId = liftIO . R.getAppId . unXdg
