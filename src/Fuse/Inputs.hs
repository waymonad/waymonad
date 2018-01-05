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
module Fuse.Inputs
    ( inputsDir
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Map (Map)
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import Fuse.Common

import Graphics.Wayland.WlRoots.Input (InputDevice, getDeviceName, inputDeviceType)

import Input
import Waymonad
import Waymonad.Types (Compositor (compInput))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T


makeInputDir :: Ptr InputDevice -> Way a (String, Entry a)
makeInputDir ptr = do
    let deviceType =
            [ ("type", FileEntry $ textFile $ liftIO $
                T.pack . show <$> (inputDeviceType ptr))
            ]
    name <- liftIO $ getDeviceName ptr
    pure (T.unpack name, DirEntry $ simpleDir $ M.fromList $ deviceType)


enumerateInputs :: Way a (Map String (Entry a))
enumerateInputs = do
    inputRef <- inputDevices . compInput . wayCompositor <$> getState
    inputs <- liftIO $ readIORef inputRef
    M.fromList <$> mapM makeInputDir (S.toList inputs)


makeFooDir :: (Text, SeatFoo) -> Way a (String, Entry a)
makeFooDir (name, foo) = do

    devPtrs <- liftIO $ readIORef $ fooDevices foo
    let makeDevLink ptr = do
            devName <- liftIO $ T.unpack <$> getDeviceName ptr
            pure (devName, SymlinkEntry $ pure $ "../../../devices/" ++ devName)
    let devDir = ("devices", DirEntry $ enumeratingDir $ fmap M.fromList $ mapM makeDevLink $ S.toList devPtrs)

    pure (T.unpack $ name, DirEntry $ simpleDir $ M.fromList [devDir])

enumerateSeats :: Way a (Map String (Entry a))
enumerateSeats = do
    fooRef <- inputFooMap . compInput . wayCompositor <$> getState
    foos <- liftIO $ readIORef fooRef
    M.fromList <$> mapM makeFooDir (M.toList foos)

inputsDir :: Entry a
inputsDir =
    DirEntry $ simpleDir $ M.fromList
        [ ("devices", DirEntry $ enumeratingDir enumerateInputs)
        , ("seats", DirEntry $ enumeratingDir enumerateSeats)
        ]
