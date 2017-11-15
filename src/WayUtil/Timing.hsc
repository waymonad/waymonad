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
module WayUtil.Timing
    ( getTime
    , setBaseTime
    , getBasedTime
    , getSeconds
    )
where

#include <time.h>

import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.C.Types (CLong, CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)

import Waymonad
import Waymonad.Extensible
import WayUtil

foreign import ccall "clock_gettime" c_gettime :: CInt -> Ptr time -> IO CInt

-- Newtype for time stored in nanoseconds since unspecified base
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

getTime :: MonadIO m => m Time
getTime = liftIO $ alloca $ \ptr -> do
    _ <- c_gettime #{const CLOCK_MONOTONIC} ptr
    peek ptr

setBaseTime :: Way a ()
setBaseTime = setEState =<< getTime

getBasedTime :: Way a Time
getBasedTime = do
    current <- getTime
    base <- getEState
    pure $ current - base

getSeconds :: Num a => Time -> a
getSeconds = fromIntegral . flip div 10e9
