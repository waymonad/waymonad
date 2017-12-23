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
{-# LANGUAGE InstanceSigs #-}
module View.Proxy
    ( makeProxy
    , ProxiedView
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Bits (Bits(complement))
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)

import View

data ProxiedView = ProxiedView { removeProxy :: View -> IO (),  unProxy :: View, myView :: IORef View}

makeProxy :: MonadIO m => View -> (View -> IO ()) -> m View
makeProxy v fun = liftIO $ do
    ref <- newIORef undefined
    new <- createView (ProxiedView fun v ref)
    writeIORef ref new
    addViewDestroyListener (getViewID new) (const $ closeView new) v
    pure new

instance ShellSurface ProxiedView where
    getSurface :: MonadIO m => ProxiedView -> m (Ptr WlrSurface)
    getSurface = getViewSurface . unProxy
    getSize :: MonadIO m => ProxiedView -> m (Double, Double)
    getSize = getViewSize . unProxy
    resize :: MonadIO m => ProxiedView -> Word32 -> Word32 -> m ()
    resize _ _ _ = pure ()
    activate :: MonadIO m => ProxiedView -> Bool -> m ()
    activate = activateView . unProxy

    close :: MonadIO m => ProxiedView -> m ()
    close (ProxiedView {removeProxy = fun, myView = ref, unProxy = view}) = liftIO $ do
        self <- readIORef ref
        fun self
        rmViewDestroyListener (getViewID self) view
        triggerViewDestroy self

    renderAdditional :: MonadIO m => (Ptr WlrSurface -> WlrBox -> m ()) -> ProxiedView -> m ()
    renderAdditional f (ProxiedView {unProxy = v}) = renderViewAdditional f v

    getEventSurface :: MonadIO m => ProxiedView -> Double -> Double -> m (Maybe (Ptr WlrSurface, Double, Double))
    getEventSurface (ProxiedView {unProxy = v}) = getViewEventSurface v
    setPosition :: MonadIO m => ProxiedView -> Double -> Double -> m ()
    setPosition _ _ _ = pure ()

    getID :: ProxiedView -> Int
    getID = complement . getViewID . unProxy
    getTitle :: MonadIO m => ProxiedView -> m Text
    getTitle = getViewTitle . unProxy
    getAppId :: MonadIO m => ProxiedView -> m Text
    getAppId = undefined

