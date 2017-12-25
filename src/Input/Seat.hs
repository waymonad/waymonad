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
    ( Seat (seatRoots, seatName, seatLoadScale)
    , seatCreate
    , keyboardEnter
    , pointerMotion
    , pointerClear
    , pointerButton
    , getPointerFocus
    , getKeyboardFocus
    , keyboardClear
    )
where

import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Maybe (isJust)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)

import Graphics.Wayland.WlRoots.Input.Pointer (WlrEventPointerButton (..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.Server (DisplayServer, seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer)

import Utility (doJust)
import View (View, getViewSurface, getViewEventSurface)

import qualified Graphics.Wayland.WlRoots.Seat as R

data Seat = Seat
    { seatRoots          :: Ptr R.WlrSeat
    , seatPointer        :: IORef (Maybe View)
    , seatKeyboard       :: IORef (Maybe View)
    , seatFocusView      :: Seat -> View -> IO ()
    , seatName           :: String
    , seatRequestDefault :: IO ()
    , seatLoadScale      :: Float -> IO ()
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
    -> IO ()
    -> (Float -> IO ())
    -> m Seat
seatCreate dsp name focus reqDefault loadScale = liftIO $ do
    roots    <- R.createSeat dsp name
    pointer  <- newIORef Nothing
    keyboard <- newIORef Nothing

    R.setSeatCapabilities roots [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]

    pure $ Seat
        { seatRoots          = roots
        , seatPointer        = pointer
        , seatKeyboard       = keyboard
        , seatFocusView      = focus
        , seatName           = name
        , seatRequestDefault = reqDefault
        , seatLoadScale      = loadScale
        }

keyboardEnter' :: MonadIO m => Seat -> Ptr WlrSurface -> View -> m Bool
keyboardEnter' seat surf view = liftIO $ do
    prev <- R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat
    R.keyboardNotifyEnter (seatRoots seat) surf
    post <- R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat

    if prev /= post
       then do
            oldView <- readIORef (seatKeyboard seat)
            let changed = oldView /= Just view
            when changed $ writeIORef (seatKeyboard seat) (Just view)
            pure changed
    else pure False

keyboardEnter :: MonadIO m => Seat -> View -> m Bool
keyboardEnter seat view = liftIO $ do
    surf <- getViewSurface view
    case surf of
        Just s -> keyboardEnter' seat s view
        Nothing -> pure False

pointerButton :: MonadIO m => Seat -> View -> Double -> Double -> WlrEventPointerButton -> m Bool
pointerButton seat view baseX baseY event = liftIO $ do
    let time = (fromIntegral $ eventPointerButtonTime event)
    R.pointerNotifyButton (seatRoots seat) time (eventPointerButtonButton event) (eventPointerButtonState event)

    evtSurf <- getViewEventSurface view baseX baseY
    case evtSurf of
        Just (surf, _, _) -> do
            changed <- keyboardEnter' seat surf view
            when changed (seatFocusView seat seat view)
            pure changed
        Nothing -> pure False

pointerEnter :: MonadIO m => Seat -> Ptr WlrSurface -> View -> Double -> Double -> m Bool
pointerEnter seat surf view x y = liftIO $ do
    prev <- R.getPointerFocus . R.getPointerState $ seatRoots seat
    R.pointerNotifyEnter (seatRoots seat) surf x y
    post <- R.getPointerFocus . R.getPointerState $ seatRoots seat

    if prev /= post
        then do
            oldView <- readIORef (seatPointer seat)
            let changed = oldView /= Just view
            when changed $ writeIORef (seatPointer seat) (Just view)
            pure changed
        else pure False

pointerMotion :: MonadIO m => Seat -> View -> Word32 -> Double -> Double -> m (Maybe View)
pointerMotion seat view time baseX baseY = liftIO $ do
    doJust (getViewEventSurface view baseX baseY) $ \(surf, x, y) -> do
        changed <- pointerEnter seat surf view x y
        R.pointerNotifyMotion (seatRoots seat) time x y
        pure $ if changed
            then Just view
            else Nothing

pointerClear :: MonadIO m => Seat -> m ()
pointerClear seat = liftIO $ do
    R.pointerClearFocus (seatRoots seat)
    post <- R.getPointerFocus . R.getPointerState $ seatRoots seat

    when (nullPtr == post) $ do
        getDefault <- (isJust <$> readIORef (seatPointer seat))
        when getDefault (seatRequestDefault seat)
        writeIORef (seatPointer seat) Nothing

getPointerFocus :: MonadIO m => Seat -> m (Maybe View)
getPointerFocus = liftIO . readIORef . seatPointer

getKeyboardFocus :: MonadIO m => Seat -> m (Maybe View)
getKeyboardFocus = liftIO . readIORef . seatKeyboard

keyboardClear :: MonadIO m => Seat -> m ()
keyboardClear seat = liftIO $ do
    R.keyboardClearFocus (seatRoots seat)
    post <- R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat

    when (post == nullPtr) $ writeIORef (seatKeyboard seat) Nothing
