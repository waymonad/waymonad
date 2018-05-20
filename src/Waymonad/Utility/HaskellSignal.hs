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
module Waymonad.Utility.HaskellSignal
    ( HaskellSignalToken
    , HaskellSignal
    , addHaskellListener
    , makeHaskellSignal
    , removeHaskellListener
    , emitHaskellSignal
    )
where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IntMap (IntMap)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

import qualified Data.IntMap as IM

data HaskellSignalToken v m = HaskellSignalToken Int (HaskellSignal v m)
data HaskellSignal v m = HaskellSignal { _hsSignalMap :: IORef (IntMap (v -> m ())), _hsSignalCounter :: IORef Int }

makeHaskellSignal :: MonadIO m => m (HaskellSignal v n)
makeHaskellSignal = liftIO (HaskellSignal <$> newIORef mempty <*> newIORef 0)

addHaskellListener :: MonadIO m => HaskellSignal v n -> (v -> n ()) -> m (HaskellSignalToken v n)
addHaskellListener s@(HaskellSignal mapRef counter) act = liftIO $ do
    value <- readIORef counter
    writeIORef counter (value + 1)

    modifyIORef mapRef (IM.insert value act)
    pure $ HaskellSignalToken value s

removeHaskellListener :: MonadIO m => HaskellSignalToken v n -> m ()
removeHaskellListener (HaskellSignalToken v (HaskellSignal refMap _)) = liftIO $
    modifyIORef refMap (IM.delete v)

emitHaskellSignal :: MonadIO m => v -> HaskellSignal v m -> m ()
emitHaskellSignal v (HaskellSignal mapRef _) = do
    lMap <- liftIO $ readIORef mapRef
    mapM_ ($ v) $ IM.elems lMap
