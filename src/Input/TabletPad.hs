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
module Input.TabletPad
where

import System.IO

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

import Graphics.Wayland.Signal (removeListener)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Pointer (AxisOrientation (..))
import Graphics.Wayland.WlRoots.Input.TabletPad
    ( WlrTabletPad
    , PadStripEvent (..)
    , PadEvents (..)
    , getPadEvents
    , pokePadData
    , peekPadData
    )

import Input.Seat
import ViewSet (WSTag)
import WayUtil.Signal (setSignalHandler)
import Waymonad.Types (Way)

--pointerAxis :: MonadIO m => Seat -> Word32 -> AxisOrientation -> Double -> m ()
--data PadStripEvent = PadStripEvent
--    { padStripEvtTime     :: Word32
--    , padStripEvtSource   :: PadStripSource
--    , padStripEvtStrip    :: Word32
--    , padStripEvtPosition :: Double
--    }

handlePadStrip :: Seat -> Ptr PadStripEvent -> Way a ()
handlePadStrip seat evt_ptr = do
    event <- liftIO $ peek evt_ptr
    liftIO $ hPutStrLn stderr $ "Strip event: " ++ show event

    pointerAxis seat (padStripEvtTime event) AxisVertical (padStripEvtPosition event)

handlePadAdd :: WSTag a => Seat -> Ptr InputDevice -> Ptr WlrTabletPad -> Way a ()
handlePadAdd seat _ pad = do
    let events = getPadEvents pad

    stripToken <- setSignalHandler (padEventStrip events) $ handlePadStrip seat

    liftIO $ do
        sptr <- newStablePtr (stripToken)
        pokePadData pad (castStablePtrToPtr sptr)

handlePadRemove :: MonadIO m => Ptr WlrTabletPad -> m ()
handlePadRemove ptr = liftIO $ do
    dptr <- peekPadData ptr
    when (dptr /= nullPtr) (do
        let sptr = castPtrToStablePtr dptr
        (tok) <- deRefStablePtr sptr
        removeListener tok
        freeStablePtr $ sptr
                           )
    pokePadData ptr nullPtr
