{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2018  Markus Ongyerth

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
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Input.TabletPad
where

import System.IO
import Data.Word (Word32)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.Storable (Storable (..))
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    , deRefStablePtr
    )

import Graphics.Wayland.Signal (removeListener, ListenerToken)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons
import Graphics.Wayland.WlRoots.Input.Keyboard (keyStateFromButtonState)
import Graphics.Wayland.WlRoots.Input.Pointer (AxisOrientation (..))
import Graphics.Wayland.WlRoots.Input.TabletPad
    ( WlrTabletPad
    , PadStripEvent (..)
    , PadButtonEvent (..)
    , PadEvents (..)
    , getPadEvents
    , pokePadData
    , peekPadData
    )
import Graphics.Wayland.WlRoots.Seat (keyboardNotifyKey)
import Text.XkbCommon.KeycodeList

import Waymonad.Input.Seat
import Waymonad.ViewSet (WSTag)
import Waymonad.Utility.Signal (setSignalHandler)
import Waymonad.Types (Way)
import Text.XkbCommon.InternalTypes (CKeycode)

--pointerAxis :: MonadIO m => Seat -> Word32 -> AxisOrientation -> Double -> m ()
--data PadStripEvent = PadStripEvent
--    { padStripEvtTime     :: Word32
--    , padStripEvtSource   :: PadStripSource
--    , padStripEvtStrip    :: Word32
--    , padStripEvtPosition :: Double
--    }

handlePadStrip :: Seat -> Ptr PadStripEvent -> Way vs a ()
handlePadStrip seat evt_ptr = do
    event <- liftIO $ peek evt_ptr
    liftIO $ hPutStrLn stderr $ "Strip event: " ++ show event

    pointerAxis seat (padStripEvtTime event) AxisVertical (padStripEvtPosition event)

handlePadButton :: Seat -> (Word32 -> CKeycode) -> Ptr PadButtonEvent -> Way vs a ()
handlePadButton seat mapFun evt_ptr = do
    event <- liftIO $ peek evt_ptr
    liftIO $ hPrint stderr event
    liftIO $ tellClient seat (padButtonEvtTime event) (fromIntegral $ toEvdev $ mapFun $ padButtonEvtButton event) (padButtonEvtState event)

handlePadAdd :: WSTag a => Seat -> Ptr InputDevice -> Ptr WlrTabletPad -> Way vs a ()
handlePadAdd seat _ pad = do
    let events = getPadEvents pad
    stripToken <- setSignalHandler (padEventStrip events) $ handlePadStrip seat
    buttonToken <- setSignalHandler (padEventButton events) $ handlePadButton seat (const keycode_c)

    liftIO $ do
        sptr <- newStablePtr [stripToken, buttonToken]
        pokePadData pad (castStablePtrToPtr sptr)

tellClient :: Seat -> Word32 -> Word32 -> ButtonState -> IO ()
tellClient seat time key state = do
    keyboardNotifyKey (seatRoots seat) time key (keyStateFromButtonState state)


handlePadRemove :: MonadIO m => Ptr WlrTabletPad -> m ()
handlePadRemove ptr = liftIO $ do
    dptr <- peekPadData ptr
    when (dptr /= nullPtr) (do
        let sptr = castPtrToStablePtr dptr
        tok :: [ListenerToken] <- deRefStablePtr sptr
        mapM_ removeListener tok
        freeStablePtr sptr
                           )
    pokePadData ptr nullPtr
