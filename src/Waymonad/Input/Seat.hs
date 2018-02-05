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
module Waymonad.Input.Seat
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
    , updatePointerFocus
    , seatDestroy
    , setPointerPosition
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Data.Maybe (isJust, fromMaybe)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)

import Graphics.Wayland.WlRoots.Box (Point)
import Graphics.Wayland.WlRoots.Render.Color (Color (..))
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

import {-# SOURCE #-} Waymonad.Input.Cursor
import Waymonad.Input.Cursor.Type
import Utility (doJust, whenJust)
import View (getViewSurface, getViewEventSurface)
import ViewSet (WSTag, FocusCore (..))
import Waymonad (getState)
import Waymonad.Types
import Waymonad.Types.Core
import WayUtil.Current (getCurrentWS)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Graphics.Wayland.WlRoots.Seat as R

seatDestroy :: Seat -> IO ()
seatDestroy Seat {seatRoots = roots} = do
    R.pointerClearFocus roots
    R.keyboardClearFocus roots
    R.destroySeat roots

seatCreate
    :: DisplayServer
    -> String
    -> IO ()
    -> (Float -> IO ())
    -> Cursor
    -> Way vs ws Seat
seatCreate dsp name reqDefault loadScale cursor = do
    roots    <- liftIO $ R.createSeat dsp name
    pointer  <- liftIO $ newIORef Nothing
    keyboard <- liftIO $ newIORef Nothing

    liftIO $ R.setSeatCapabilities roots [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]
    cMap <- waySeatColors <$> getState
    pure Seat
        { seatRoots          = roots
        , seatPointer        = pointer
        , seatKeyboard       = keyboard
        , seatName           = name
        , seatRequestDefault = reqDefault
        , seatLoadScale      = loadScale
        , seatCursor         = cursor
        , seatColor          = fromMaybe (Color 0 1 0 1) $ M.lookup (T.pack name) cMap
        }

keyboardEnter' :: Seat -> Ptr WlrSurface -> View -> Way vs ws Bool
keyboardEnter' seat surf view = do
    keyM <- liftIO $ R.getSeatKeyboard $ seatRoots seat
    case keyM of
        Nothing -> pure False
        Just keyboard -> do
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

updatePointerFocus :: (FocusCore vs ws, WSTag ws) => Seat -> Way vs ws ()
updatePointerFocus seat = updateFocus (seatCursor seat) 0

setPointerPosition :: (FocusCore vs ws, WSTag ws) => Seat -> (Double, Double) -> Way vs ws ()
setPointerPosition seat p = forcePosition (seatCursor seat) p 0
