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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Waymonad.Utility.Timing
Description : Base module to provide timing for other modules
Maintainer  : ongy
Stability   : testing
Portability : Linux

This modules is supposed to be used by others to provide functionality that
depends on the time passed since the compositor started.

The main usage is over 'getBasedTime' which provides the time in nanoseconds
since the compositor started (roughly).

A common usage in other code would be:

>seconds <- getSeconds <$> getBasedtime
>if seconds < 300
>    then -- When in first 5 minutes --
>    else -- otherwise --

-}
module Waymonad.Utility.Timing
    ( Time
    , getTime
    , setBaseTime
    , getBasedTime
    , getSeconds
    , baseTimeBracket
    )
where

#include <time.h>

import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.C.Types (CLong, CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Server (DisplayServer)

import Waymonad.Start (Bracketed (..))
import Waymonad
import Waymonad.Extensible
import Waymonad.Utility.Extensible

foreign import ccall "clock_gettime" c_gettime :: CInt -> Ptr time -> IO CInt

-- |Newtype for time stored in nanoseconds since unspecified base
newtype Time = Time Integer
    deriving (Show, Eq, Num, Real, Integral, Ord, Enum)

instance Storable Time where
    alignment _ = #{alignment struct timespec}
    sizeOf _ = #{size struct timespec}
    peek ptr = do
        secs :: Word <- #{peek struct timespec, tv_sec} ptr
        nsecs :: CLong <- #{peek struct timespec, tv_nsec} ptr
        pure $ Time (fromIntegral secs * 10e9 + fromIntegral nsecs)
    poke ptr (Time val) = do
        let nsecs :: CLong = fromIntegral (val `mod` 10e9)
            secs :: Word = fromIntegral (val `div` 10e9)
        #{poke struct timespec, tv_sec} ptr secs
        #{poke struct timespec, tv_nsec} ptr nsecs

instance ExtensionClass Time where
    initialValue = 0

-- | Get the current time as 'Time' (this is not relative to a defined base)
getTime :: MonadIO m => m Time
getTime = liftIO $ alloca $ \ptr -> do
    _ <- c_gettime #{const CLOCK_MONOTONIC} ptr
    peek ptr

-- | Set the current time as the base time, 'getBasedTime' should be based on
setBaseTime :: Way vs a ()
setBaseTime = setEState =<< getTime

-- | Get the time elapsed since a previous point as base. 'baseTimeBracket'
-- should be used to set it at the compositor startup.
getBasedTime :: Way vs a Time
getBasedTime = do
    current <- getTime
    base <- getEState
    pure $ current - base

-- | Convert the 'Time' value into seconds
getSeconds :: Num a => Time -> a
getSeconds = fromIntegral . flip div 10e9

-- | Set the current time as base time for 'getBasedTime', this should be used
-- to get roughly the time since compositor startup.
baseTimeBracket :: Bracketed vs DisplayServer ws
baseTimeBracket = Bracketed (const setBaseTime) (const $ pure ())
