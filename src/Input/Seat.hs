module Input.Seat
    ( Seat (seatRoots)
    , seatCreate
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Seat (WlrSeat, createSeat)
import Graphics.Wayland.Server (DisplayServer)

import View (View)

data Seat = Seat
    { seatRoots    :: Ptr WlrSeat
-- Should those be Maybe View?
    , seatPointer  :: IORef (Maybe View)
    , seatKeyboard :: IORef (Maybe View)
    }

seatCreate
    :: MonadIO m
    => DisplayServer
    -> String
    -> m Seat
seatCreate dsp name = do
    roots    <- liftIO $ createSeat dsp name
    pointer  <- liftIO $ newIORef Nothing
    keyboard <- liftIO $ newIORef Nothing

    pure $ Seat
        { seatRoots    = roots
        , seatPointer  = pointer
        , seatKeyboard = keyboard
        }
