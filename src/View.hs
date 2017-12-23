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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module View
    ( ShellSurface (..)
    , View (..)
    , getViewSize
    , getViewBox
    , createView
    , moveView
    , resizeView
    , getViewSurface
    , activateView
    , renderViewAdditional
    , getViewEventSurface
    , setViewBox
    , closeView
    , getViewClient
    , getViewInner
    , getViewTitle
    , setViewLocal
    , viewGetScale
    , viewGetLocal
    )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Resource (resourceGetClient)
import Graphics.Wayland.Server (Client)

import Graphics.Wayland.WlRoots.Surface (WlrSurface, getSurfaceResource, subSurfaceAt)
import Graphics.Wayland.WlRoots.Box (WlrBox(..), toOrigin, centerBox)

class Typeable a => ShellSurface a where
    getSurface :: MonadIO m => a -> m (Ptr WlrSurface)
    getSize :: MonadIO m => a -> m (Double, Double)
    resize :: MonadIO m => a -> Word32 -> Word32 -> m ()
    activate :: MonadIO m => a -> Bool -> m ()
    close :: MonadIO m => a -> m ()
    renderAdditional :: MonadIO m => (Ptr WlrSurface -> WlrBox -> m ()) -> a -> m ()
    renderAdditional _ _ = pure ()
    getEventSurface :: MonadIO m => a -> Double -> Double -> m (Maybe (Ptr WlrSurface, Double, Double))
    setPosition :: MonadIO m => a -> Double -> Double -> m ()
    setPosition _ _ _ = pure ()
    getID :: a -> Int
    getTitle :: MonadIO m => a -> m Text
    getAppId :: MonadIO m => a -> m Text

data View = forall a. ShellSurface a => View
    { viewSurface  :: a
    , viewBox      :: IORef WlrBox
    , viewPosition :: IORef WlrBox
    , viewScaling  :: IORef Float
    }

instance Show View where
    show (View {viewSurface=v}) = show $ getID v

instance Ord View where
    compare (View {viewSurface=left}) (View {viewSurface=right}) = compare (getID left) (getID right)

instance Eq View where
    (View {viewSurface=left}) == (View {viewSurface=right}) =
        getID left == getID right

getViewSize :: MonadIO m => View -> m (Double, Double)
getViewSize (View {viewSurface=surf}) = getSize surf

getViewBox :: MonadIO m => View -> m WlrBox
getViewBox = liftIO . readIORef . viewBox

setViewBox :: MonadIO m => View -> WlrBox -> m ()
setViewBox v box = do
    moveView v (fromIntegral $ boxX box) (fromIntegral $ boxY box)
    resizeView v (fromIntegral $ boxWidth box) (fromIntegral $ boxHeight box)
    liftIO $ writeIORef (viewBox v) box

createView :: (ShellSurface a, MonadIO m) => a -> m View
createView surf = do
    (width, height) <- getSize surf
    let box = WlrBox 0 0 (floor width) (floor height)
    global <- liftIO $ newIORef box
    local <- liftIO $ newIORef box
    scale <- liftIO $ newIORef 1.0
    pure View
        { viewSurface = surf
        , viewBox = global
        , viewPosition = local
        , viewScaling = scale
        }

closeView :: MonadIO m => View -> m ()
closeView (View {viewSurface=surf}) = close surf

moveView :: MonadIO m => View -> Double -> Double -> m ()
moveView (View {viewSurface = surf, viewBox = ref}) x y = do
    old <- liftIO $ readIORef ref
    let new = old { boxX = floor x, boxY = floor y}
    liftIO $ writeIORef ref new
    setPosition surf x y


resizeView :: MonadIO m => View -> Double -> Double -> m ()
resizeView v@(View {viewSurface = surf, viewBox = ref}) width height = do
    old <- liftIO $ readIORef ref
    let new = old { boxWidth = floor width, boxHeight = floor height}
    liftIO $ writeIORef ref new
    (oldWidth, oldHeight) <- getSize surf
    resize surf (floor width) (floor height)

    setViewLocal v $ WlrBox 0 0 (floor oldWidth) (floor oldHeight)

getViewSurface :: MonadIO m => View -> m (Ptr WlrSurface)
getViewSurface (View {viewSurface = surf}) = getSurface surf


activateView :: MonadIO m => View -> Bool -> m ()
activateView (View {viewSurface = surf}) active = activate surf active


renderViewAdditional :: MonadIO m => (Ptr WlrSurface -> WlrBox -> m ()) -> View -> m ()
renderViewAdditional fun (View {viewSurface = surf}) = do
    renderAdditional fun surf


getViewEventSurface :: MonadIO m => View -> Double -> Double -> m (Maybe (Ptr WlrSurface, Double, Double))
getViewEventSurface (View {viewSurface = surf, viewPosition = local, viewScaling = scale}) x y = liftIO $ do
    scaleFactor <- readIORef scale
    posBox <- readIORef local
    evtSurf <- getEventSurface surf
        ((x - fromIntegral (boxX posBox)) / realToFrac scaleFactor)
        ((y - fromIntegral (boxY posBox)) / realToFrac scaleFactor)
    case evtSurf of
        Nothing -> pure Nothing
        Just tmp@(topSurf, eX, eY) -> do
            ret <- subSurfaceAt topSurf eX eY
            pure (ret <|> Just tmp)

getViewClient :: MonadIO m => View -> m (Maybe Client)
getViewClient (View {viewSurface = surf}) = do
    res <- liftIO . getSurfaceResource =<< getSurface surf
    Just <$> liftIO (resourceGetClient res)

getViewInner :: Typeable a => View -> Maybe a
getViewInner (View {viewSurface = surf}) = cast surf

getViewTitle :: MonadIO m => View -> m Text
getViewTitle (View {viewSurface = surf}) = getTitle surf

getLocalBox :: WlrBox -> WlrBox -> (WlrBox, Float)
getLocalBox inner outer =
    if boxWidth inner <= boxWidth outer && boxHeight inner <= boxHeight outer
        then (inner, 1.0)
        else
            let scale:: Float = min (fromIntegral (boxWidth  outer) / fromIntegral (boxWidth  inner))
                                    (fromIntegral (boxHeight outer) / fromIntegral (boxHeight inner))

             in (WlrBox 0 0 (floor $ scale * fromIntegral (boxWidth inner)) (floor $ scale * fromIntegral (boxHeight inner)), scale)

-- | This should be called whenever the contained surface is resized. It will
-- determine whether we should try and downscale it to fit the area, or
-- position it somewhere inside the configured box, because it is *smaller*
-- than the intended area
setViewLocal :: MonadIO m => View -> WlrBox -> m ()
setViewLocal (View {viewBox = global, viewPosition = local, viewScaling = scaleRef}) box = liftIO $ do
    outerBox <- readIORef global
    if toOrigin outerBox == box
        then do
            writeIORef local box
            writeIORef scaleRef 1
        else do
            let (inner, scale) = getLocalBox box outerBox
            writeIORef local (centerBox inner $ toOrigin outerBox)
            writeIORef scaleRef scale

viewGetScale :: MonadIO m => View -> m Float
viewGetScale (View {viewScaling = scale}) = liftIO $ readIORef scale

viewGetLocal :: MonadIO m => View -> m WlrBox
viewGetLocal (View {viewPosition = local}) = liftIO $ readIORef local
