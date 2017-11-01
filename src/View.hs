{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module View
    ( ShellSurface (..)
    , View (..)
    , getViewBox
    , createView
    , moveView
    , resizeView
    , getViewSurface
    , activateView
    , renderViewAdditional
    , getViewEventSurface
    , setViewBox
    )
where

import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Monad.IO.Class
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Box (WlrBox(..))

class ShellSurface a where
    getSurface :: MonadIO m => a -> m (Ptr WlrSurface)
    getSize :: MonadIO m => a -> m (Double, Double)
    resize :: MonadIO m => a -> Word32 -> Word32 -> m ()
    activate :: MonadIO m => a -> Bool -> m ()
    close :: MonadIO m => a -> m ()
    renderAdditional :: MonadIO m => (Ptr WlrSurface -> Int -> Int -> m ()) -> a -> Int -> Int -> m ()
    renderAdditional _ _ _ _ = pure ()
    getEventSurface :: MonadIO m => a -> Double -> Double -> m (Ptr WlrSurface, Double, Double)
    setPosition :: MonadIO m => a -> Double -> Double -> m ()
    setPosition _ _ _ = pure ()
    getID :: a -> Int

data View = forall a. ShellSurface a => View
    { viewX :: IORef Double
    , viewY :: IORef Double
    , viewSurface :: a
    }

instance Eq View where
    (View _ _ left) == (View _ _ right) =
        getID left == getID right

getViewBox :: MonadIO m => View -> m WlrBox
getViewBox (View xref yref surf) = do
    (width, height) <- getSize surf
    x <- liftIO $ readIORef xref
    y <- liftIO $ readIORef yref
    pure WlrBox
        { boxX = floor x
        , boxY = floor y
        , boxWidth  = floor width
        , boxHeight = floor height
        }

setViewBox :: MonadIO m => View -> WlrBox -> m ()
setViewBox v box = do
    moveView v (fromIntegral $ boxX box) (fromIntegral $ boxY box)
    resizeView v (fromIntegral $ boxWidth box) (fromIntegral $ boxHeight box)

createView :: (ShellSurface a, MonadIO m) => a -> m View
createView surf = do
    xref <- liftIO $ newIORef 0
    yref <- liftIO $ newIORef 0
    pure View
        { viewX = xref
        , viewY = yref
        , viewSurface = surf
        }


moveView :: MonadIO m => View -> Double -> Double -> m ()
moveView (View xref yref surf) x y = do
    liftIO $ writeIORef xref x
    liftIO $ writeIORef yref y
    setPosition surf x y


resizeView :: MonadIO m => View -> Double -> Double -> m ()
resizeView (View _ _ surf) width height = resize surf (floor width) (floor height)


getViewSurface :: MonadIO m => View -> m (Ptr WlrSurface)
getViewSurface (View _ _ surf) = getSurface surf


activateView :: MonadIO m => View -> Bool -> m ()
activateView (View _ _ surf) active = activate surf active


renderViewAdditional :: MonadIO m => (Ptr WlrSurface -> Int -> Int -> m ()) -> View -> m ()
renderViewAdditional fun (View xref yref surf) = do
    x <- liftIO $ readIORef xref
    y <- liftIO $ readIORef yref
    renderAdditional fun surf (floor x) (floor y)


getViewEventSurface :: MonadIO m => View -> Double -> Double -> m (Ptr WlrSurface, Double, Double)
getViewEventSurface (View xref yref surf) x y = do
    ownX <- liftIO $ readIORef xref
    ownY <- liftIO $ readIORef yref
    getEventSurface surf (x - ownX) (y - ownY)
