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
module Input.Seat
    ( Seat (seatRoots, seatName)
    , seatCreate
    , keyboardEnter
    , pointerMotion
    , pointerClear
    , pointerButton
    , getPointerFocus
    , getKeyboardFocus
    )
where

import Control.Monad (when, void)
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

import Utility (doJust)
import View (View, getViewSurface, getViewEventSurface)

data Seat = Seat
    { seatRoots     :: Ptr WlrSeat
    , seatPointer   :: IORef (Maybe View)
    , seatKeyboard  :: IORef (Maybe View)
    , seatFocusView :: Seat -> View -> IO ()
    , seatName      :: String
    }

instance Show Seat where
    show = seatName

instance Eq Seat where
    l == r = seatRoots l == seatRoots r

instance Ord Seat where
    l `compare` r = seatRoots l `compare` seatRoots r

seatCreate
    :: MonadIO m
    => DisplayServer
    -> String
    -> (Seat -> View -> IO ())
    -> m Seat
seatCreate dsp name focus = liftIO $ do
    roots    <- createSeat dsp name
    pointer  <- newIORef Nothing
    keyboard <- newIORef Nothing

    setSeatCapabilities roots [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]

    pure $ Seat
        { seatRoots     = roots
        , seatPointer   = pointer
        , seatKeyboard  = keyboard
        , seatFocusView = focus
        , seatName = name
        }

keyboardEnter' :: MonadIO m => Seat -> Ptr WlrSurface -> View -> m Bool
keyboardEnter' seat surf view = liftIO $ do
    -- TODO: I should probably check what wlroots actually does here in case of
    -- grabs
    liftIO $ keyboardNotifyEnter (seatRoots seat) surf
    prev <- readIORef (seatKeyboard seat)
    let changed = prev /= Just view
    when changed $ do
        writeIORef (seatKeyboard seat) (Just view)
    pure changed

keyboardEnter :: MonadIO m => Seat -> View -> m ()
keyboardEnter seat view = liftIO $ do
    surf <- getViewSurface view
    void $ keyboardEnter' seat surf view

pointerButton :: MonadIO m => Seat -> View -> Double -> Double -> WlrEventPointerButton -> m ()
pointerButton seat view baseX baseY event = liftIO $ do
    let time = (fromIntegral $ eventPointerButtonTime event)
    pointerNotifyButton (seatRoots seat) time (eventPointerButtonButton event) (eventPointerButtonState event)

    doJust (getViewEventSurface view baseX baseY) $ \(surf, _, _) -> do
        changed <- keyboardEnter' seat surf view
        when changed (seatFocusView seat seat view)

pointerEnter :: MonadIO m => Seat -> Ptr WlrSurface -> View -> Double -> Double -> m ()
pointerEnter seat surf view x y = liftIO $ do
    pointerNotifyEnter (seatRoots seat) surf x y
    writeIORef (seatPointer seat) (Just view)

pointerMotion :: MonadIO m => Seat -> View -> Word32 -> Double -> Double -> m ()
pointerMotion seat view time baseX baseY = liftIO $ do
    doJust (getViewEventSurface view baseX baseY) $ \(surf, x, y) -> do
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
