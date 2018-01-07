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
module XdgShell
    ( xdgShellCreate
    , XdgShell
    )
where

import Control.Monad (forM)

import Utility (doJust)
import View
import Waymonad
import WayUtil.Signal (setSignalHandler)
import WayUtil.Log (logPutText, LogPriority (..))
import Control.Monad (filterM, forM_, unless)
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Data.Composition ((.:))

import Graphics.Wayland.WlRoots.Box (WlrBox (..), Point (..), boxContainsPoint)
import Graphics.Wayland.WlRoots.Surface -- (WlrSurface)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, ptrToIntPtr)
import qualified Graphics.Wayland.WlRoots.XdgShell as R
import Data.IORef (newIORef, IORef, modifyIORef, readIORef)
import Graphics.Wayland.Server (DisplayServer)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )
import ViewSet (WSTag)
import Managehook (insertView, removeView)

import qualified Data.IntMap.Strict as M

type MapRef =  IORef (IntMap View)

data XdgShell = XdgShell
    { xdgSurfaceRef :: MapRef
    , xdgWlrootsShell :: Ptr R.WlrXdgShell
    }


ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

newtype XdgSurface = XdgSurface { unXdg :: Ptr R.WlrXdgSurface }

xdgShellCreate
    :: WSTag a
    => DisplayServer
    -> Way a XdgShell
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
    :: WSTag a
    => MapRef
    -> Ptr R.WlrXdgSurface
    -> Way a ()
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
    :: WSTag a
    => MapRef
    -> Ptr R.WlrXdgSurface
    -> Way a ()
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
        pure $ ((Point x y), (Point (x + w) (x + h)))
    let topleft = map fst points
        botright = map snd points
        Point lx ly = foldr (\(Point x1 y1) (Point x2 y2) -> Point (min x1 x2) (min y1 y2)) (Point 0 0) topleft
        Point hx hy = foldr (\(Point x1 y1) (Point x2 y2) -> Point (max x1 x2) (max y1 y2)) (Point bw bh) botright
    pure $ (fromIntegral (hx - lx), fromIntegral (hy - ly))


instance ShellSurface XdgSurface where
    close = liftIO . R.sendClose . unXdg
    getSurface = liftIO . R.xdgSurfaceGetSurface . unXdg
    getSize (XdgSurface surf) = liftIO $ getBoundingBox surf
    resize (XdgSurface surf) width height =
        liftIO $ R.setSize surf width height
    activate = liftIO .: R.setActivated . unXdg
    renderAdditional fun (XdgSurface surf) = renderPopups fun surf
    getEventSurface (XdgSurface surf) x y = liftIO $ do
        mPop <- R.xdgPopupAt surf x y
        case mPop of
            Nothing -> do
                box <- R.getGeometry surf
                if boxContainsPoint (Point (floor x) (floor y)) box
                    then do
                        realS <- R.xdgSurfaceGetSurface surf
                        pure $ fmap (, x, y) realS
                    else pure Nothing
            Just (popup, newx, newy) -> do
                realS <- R.xdgSurfaceGetSurface popup
                pure $ fmap (,x - newx, y - newy) realS
    getID = ptrToInt . unXdg
    getTitle = liftIO . R.getTitle . unXdg
    getAppId = liftIO . R.getAppId . unXdg
