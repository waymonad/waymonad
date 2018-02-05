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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.Types.Core
where

import Control.Monad.IO.Class (MonadIO)
import Data.IntMap (IntMap)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Waymonad.Input.Cursor.Type

import Graphics.Wayland.Signal (ListenerToken)
import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Render.Color (Color)

import qualified Graphics.Wayland.WlRoots.Seat as R

data Seat = Seat
    { seatRoots          :: Ptr R.WlrSeat
    , seatPointer        :: IORef (Maybe View)
    , seatKeyboard       :: IORef (Maybe View)
    , seatName           :: String
    , seatRequestDefault :: IO ()
    , seatLoadScale      :: Float -> IO ()
    , seatCursor         :: Cursor
    , seatColor          :: Color
    }

class Typeable a => ShellSurface a where
    getSurface :: MonadIO m => a -> m (Maybe (Ptr WlrSurface))
    getSize :: MonadIO m => a -> m (Double, Double)
    resize :: MonadIO m => a -> Word32 -> Word32 -> m ()
    activate :: MonadIO m => a -> Bool -> m ()
    close :: MonadIO m => a -> m ()
    renderAdditional :: MonadIO m => (Ptr WlrSurface -> WlrBox -> m ()) -> a -> m ()
    renderAdditional _ _ = pure ()
    getEventSurface :: MonadIO m => a -> Double -> Double -> m (Maybe (Ptr WlrSurface, Double, Double))
    setPosition :: MonadIO m => a -> Double -> Double -> m ()
    setPosition _ _ _ = pure ()
    getID :: a -> Int
    getTitle :: MonadIO m => a -> m (Maybe Text)
    getAppId :: MonadIO m => a -> m (Maybe Text)

    setViewHidden :: MonadIO m => a -> m ()
    setViewHidden _ = pure ()
    setViewVisible :: MonadIO m => a -> m ()
    setViewVisible _ = pure ()
    hasCSD         :: MonadIO m => a -> m Bool

data View = forall a. ShellSurface a => View
    { viewSurface  :: a
    , viewBox      :: IORef WlrBox
    , viewPosition :: IORef WlrBox
    , viewScaling  :: IORef Float
    , viewDestroy  :: IORef (IntMap (View -> IO ()))
    , viewResize   :: IORef (IntMap (View -> IO ()))
    , viewID       :: Int
    , viewTokens   :: [ListenerToken]
    , viewDirty    :: IORef Bool
    , viewFocus    :: IORef (Maybe (Seat -> View -> IO ()))
    }

instance Show Seat where
    show = seatName

instance Eq Seat where
    l == r = seatRoots l == seatRoots r

instance Ord Seat where
    l `compare` r = seatRoots l `compare` seatRoots r


instance Show View where
    show v = show $ viewID v

instance Ord View where
    compare left right = compare (viewID left) (viewID right)

instance Eq View where
    left == right = viewID left == viewID right

