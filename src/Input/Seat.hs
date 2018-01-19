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
    , pointerAxis
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Data.Maybe (isJust)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)

import Graphics.Wayland.WlRoots.Input.Buttons (ButtonState)
import Graphics.Wayland.WlRoots.Input.Pointer (AxisOrientation)
import Graphics.Wayland.WlRoots.Input.Keyboard (getModifierPtr, getKeyboardKeys)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.Server
    ( DisplayServer
    , seatCapabilityTouch
    , seatCapabilityKeyboard
    , seatCapabilityPointer
    )

import Utility (doJust, whenJust)
import View (View, getViewSurface, getViewEventSurface)
import ViewSet (WSTag, FocusCore (..))
import Waymonad (getState)
import Waymonad.Types
import WayUtil.Current (getCurrentWS)

import qualified Graphics.Wayland.WlRoots.Seat as R

data Seat = Seat
    { seatRoots          :: Ptr R.WlrSeat
    , seatPointer        :: IORef (Maybe View)
    , seatKeyboard       :: IORef (Maybe View)
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
    -> IO ()
    -> (Float -> IO ())
    -> m Seat
seatCreate dsp name reqDefault loadScale = liftIO $ do
    roots    <- R.createSeat dsp name
    pointer  <- newIORef Nothing
    keyboard <- newIORef Nothing

    R.setSeatCapabilities roots [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]

    pure Seat
        { seatRoots          = roots
        , seatPointer        = pointer
        , seatKeyboard       = keyboard
        , seatName           = name
        , seatRequestDefault = reqDefault
        , seatLoadScale      = loadScale
        }

keyboardEnter' :: Seat -> Ptr WlrSurface -> View -> Way vs ws Bool
keyboardEnter' seat surf view = do
    keyboard <- liftIO $ R.getSeatKeyboard $ seatRoots seat
    let modifiers = getModifierPtr keyboard
    (keys, num) <- liftIO $ getKeyboardKeys keyboard
    prev <- liftIO $ R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat
    liftIO $ R.keyboardNotifyEnter (seatRoots seat) surf keys num modifiers
    post <- liftIO $ R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat

    if prev /= post || prev == surf
       then do
            oldView <- liftIO $ readIORef (seatKeyboard seat)
            let changed = oldView /= Just view
            when changed $ do
                liftIO $ writeIORef (seatKeyboard seat) (Just view)
                hook <- wayHooksSeatFocusChange . wayCoreHooks <$> getState
                hook $ KeyboardFocusChange seat oldView (Just view)
            pure changed
    else pure False

keyboardEnter :: Seat -> View -> Way vs ws Bool
keyboardEnter seat view = do
    surf <- getViewSurface view
    case surf of
        Just s -> keyboardEnter' seat s view
        Nothing -> pure False

pointerButton
    :: Seat
    -> View
    -> Double
    -> Double
    -> Word32
    -> Word32
    -> ButtonState
    -> Way vs ws ()
pointerButton seat _ _ _ time button state =
    liftIO $ R.pointerNotifyButton (seatRoots seat) time button state

-- TODO: Deduplicate this away
modifyViewSet :: WSTag ws => (vs -> vs) -> Way vs ws ()
modifyViewSet fun = do
    ref <- wayBindingState <$> getState
    liftIO $ modifyIORef ref fun

pointerEnter :: (FocusCore vs ws, WSTag ws) => Seat -> Ptr WlrSurface -> View -> Double -> Double -> Way vs ws Bool
pointerEnter seat surf view x y = do
    prev <- liftIO $ R.getPointerFocus . R.getPointerState $ seatRoots seat
    liftIO $ R.pointerNotifyEnter (seatRoots seat) surf x y
    post <- liftIO $ R.getPointerFocus . R.getPointerState $ seatRoots seat

    if prev /= post || prev == surf
        then do
            oldView <- liftIO $ readIORef (seatPointer seat)
            let changed = oldView /= Just view
            when changed $ do
                liftIO $ writeIORef (seatPointer seat) (Just view)
                ws <- getCurrentWS
                modifyViewSet (_focusView ws seat view)
                hook <- wayHooksSeatFocusChange . wayCoreHooks <$> getState
                hook $ PointerFocusChange seat oldView (Just view)
            pure changed
        else pure False

pointerMotion :: (FocusCore vs ws, WSTag ws) => Seat -> View -> Word32 -> Double -> Double -> Way vs ws (Maybe View)
pointerMotion seat view time baseX baseY =
    doJust (getViewEventSurface view baseX baseY) $ \(surf, x, y) -> do
        changed <- pointerEnter seat surf view x y
        liftIO $ R.pointerNotifyMotion (seatRoots seat) time x y
        pure $ if changed
            then Just view
            else Nothing

pointerClear :: Seat -> Way vs ws ()
pointerClear seat = do
    oldView <- liftIO $ readIORef (seatPointer seat)
    whenJust oldView $ \_ -> do
        liftIO $ R.pointerClearFocus (seatRoots seat)
        post <- liftIO $ R.getPointerFocus . R.getPointerState $ seatRoots seat

        when (nullPtr == post) $ do
            getDefault <- liftIO (isJust <$> readIORef (seatPointer seat))
            when getDefault (liftIO $ seatRequestDefault seat)
            liftIO $ writeIORef (seatPointer seat) Nothing
            hook <- wayHooksSeatFocusChange . wayCoreHooks <$> getState
            hook $ PointerFocusChange seat oldView Nothing

pointerAxis :: MonadIO m => Seat -> Word32 -> AxisOrientation -> Double -> m ()
pointerAxis seat time orientation value = liftIO $
    R.pointerNotifyAxis (seatRoots seat) time orientation value

getPointerFocus :: MonadIO m => Seat -> m (Maybe View)
getPointerFocus = liftIO . readIORef . seatPointer

getKeyboardFocus :: MonadIO m => Seat -> m (Maybe View)
getKeyboardFocus = liftIO . readIORef . seatKeyboard

keyboardClear :: Seat -> Way vs ws ()
keyboardClear seat = do
    oldView <- liftIO $ readIORef (seatKeyboard seat)
    whenJust oldView $ \_ -> do
        liftIO $ R.keyboardClearFocus (seatRoots seat)
        post <- liftIO $ R.getKeyboardFocus . R.getKeyboardState $ seatRoots seat

        when (post == nullPtr) $ do
            liftIO $ writeIORef (seatKeyboard seat) Nothing
            hook <- wayHooksSeatFocusChange . wayCoreHooks <$> getState
            hook $ KeyboardFocusChange seat oldView Nothing
