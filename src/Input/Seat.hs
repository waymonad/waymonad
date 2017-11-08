module Input.Seat
    ( Seat (seatRoots)
    , seatCreate
    , keyboardEnter
    , pointerMotion
    , pointerClear
    , pointerButton
    , getPointerFocus
    , getKeyboardFocus
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Input.Pointer (WlrEventPointerButton (..))
import Graphics.Wayland.WlRoots.Seat
    ( WlrSeat
    , createSeat
    , keyboardNotifyEnter
    , pointerClearFocus
    , pointerNotifyEnter
    , pointerNotifyMotion
    , pointerNotifyButton
    , setSeatCapabilities
    )
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.Server (DisplayServer, seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer)

import View (View, getViewSurface, getViewEventSurface)

data Seat = Seat
    { seatRoots    :: Ptr WlrSeat
-- Should those be Maybe View?
    , seatPointer  :: IORef (Maybe View)
    , seatKeyboard :: IORef (Maybe View)
    }

instance Eq Seat where
    l == r = seatRoots l == seatRoots r

instance Ord Seat where
    l `compare` r = seatRoots l `compare` seatRoots r

seatCreate
    :: MonadIO m
    => DisplayServer
    -> String
    -> m Seat
seatCreate dsp name = liftIO $ do
    roots    <- createSeat dsp name
    pointer  <- newIORef Nothing
    keyboard <- newIORef Nothing

    setSeatCapabilities roots [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]

    pure $ Seat
        { seatRoots    = roots
        , seatPointer  = pointer
        , seatKeyboard = keyboard
        }

keyboardEnter' :: MonadIO m => Seat -> Ptr WlrSurface -> View -> m ()
keyboardEnter' seat surf view = liftIO $ do
    liftIO $ keyboardNotifyEnter (seatRoots seat) surf
    writeIORef (seatKeyboard seat) (Just view)

keyboardEnter :: MonadIO m => Seat -> View -> m ()
keyboardEnter seat view = liftIO $ do
    surf <- getViewSurface view
    keyboardEnter' seat surf view

pointerButton :: MonadIO m => Seat -> View -> Double -> Double -> WlrEventPointerButton -> m ()
pointerButton seat view baseX baseY event = liftIO $ do
    let time = (fromIntegral $ eventPointerButtonTime event)
    pointerNotifyButton (seatRoots seat) time (eventPointerButtonButton event) (eventPointerButtonState event)

    Just (surf, x, y) <- getViewEventSurface view baseX baseY
    pointerEnter seat surf view x y

pointerEnter :: MonadIO m => Seat -> Ptr WlrSurface -> View -> Double -> Double -> m ()
pointerEnter seat surf view x y = liftIO $ do
    pointerNotifyEnter (seatRoots seat) surf x y
    writeIORef (seatPointer seat) (Just view)

pointerMotion :: MonadIO m => Seat -> View -> Word32 -> Double -> Double -> m ()
pointerMotion seat view time baseX baseY = liftIO $ do
    Just (surf, x, y) <- getViewEventSurface view baseX baseY
    pointerEnter seat surf view x y
    pointerNotifyMotion (seatRoots seat) time x y

pointerClear :: MonadIO m => Seat -> m ()
pointerClear seat = liftIO $ do
    pointerClearFocus (seatRoots seat)
    writeIORef (seatPointer seat) Nothing

getPointerFocus :: MonadIO m => Seat -> m (Maybe View)
getPointerFocus = liftIO . readIORef . seatPointer

getKeyboardFocus :: MonadIO m => Seat -> m (Maybe View)
getKeyboardFocus = liftIO . readIORef . seatKeyboard
