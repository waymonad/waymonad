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
module WayUtil.Floating
    ( centerFloat
    , toggleFloat
    , isFloating
    , getFloats
    , modifyFloating
    , unsetFloating
    , setFloating
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)
import Data.Set (Set)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Utility (doJust)
import View (View, moveView, resizeView)
import ViewSet (WSTag, addView, rmView)
import Waymonad
    ( Way
    , WayBindingState (..)
    , getState
    )
import WayUtil.Current (getCurrentBox, getCurrentView)
import WayUtil.ViewSet (modifyCurrentWS, modifyViewSet)

import qualified Data.Set as S

modifyFloating :: (Set View -> Set View) -> Way a ()
modifyFloating fun = liftIO . flip modifyIORef fun . wayFloating =<< getState

getFloats :: Way a (Set View)
getFloats = liftIO . readIORef . wayFloating =<< getState

isFloating :: View -> Way a Bool
isFloating v = S.member v <$> getFloats

setFloating
    :: WSTag a => View -> WlrBox -> Way a ()
setFloating view (WlrBox x y width height) = do
    moveView view (fromIntegral x) (fromIntegral y)
    resizeView view (fromIntegral width) (fromIntegral height)
    modifyFloating $ S.insert view

unsetFloating
    :: WSTag a => View -> Way a ()
unsetFloating view = do
    floats <- isFloating view
    when floats $ do
        modifyFloating $ S.delete view
        modifyCurrentWS (flip addView view . Just)

toggleFloat
    :: WSTag a => WlrBox ->  Way a ()
toggleFloat box = doJust getCurrentView $ \view -> do
    floats <- isFloating view
    if floats
        then unsetFloating view
        else do
            modifyViewSet (fmap $ rmView view)
            setFloating view box


centerFloat
    :: WSTag a => Way a ()
centerFloat = do
    (WlrBox x y w h) <- getCurrentBox
    let nw = w `div` 2
    let nh = h `div` 2
    toggleFloat $ WlrBox (x + nw `div` 2) (y + nh `div` 2) nw nh
