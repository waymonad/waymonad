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
{-# LANGUAGE OverloadedStrings #-}
module Fuse.Inputs
    ( inputsDir
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Map (Map)
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import Foreign.C.Error (Errno, eBADF)
import Fuse.Common

import Graphics.Wayland.WlRoots.Input (InputDevice, getDeviceName, inputDeviceType)

import Input
import ViewSet (WSTag, FocusCore)
import Waymonad
import Waymonad.Types (Compositor (compInput))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T


ensureWDevice :: Ptr InputDevice -> Way vs ws (Either Errno a) -> Way vs ws (Either Errno a)
ensureWDevice ptr act = do
    devs <- liftIO . readIORef . inputDevices . compInput . wayCompositor =<< getState
    if ptr `S.member` devs
        then act
        else pure $ Left eBADF

makeInputDir :: (FocusCore vs a, WSTag a) => Ptr InputDevice -> Way vs a (String, Entry vs a)
makeInputDir ptr = do
    typeStr <- liftIO $ T.pack . show <$> inputDeviceType ptr
    let deviceType =
            [ ("type", FileEntry $ textFile $ pure typeStr)
            , ("detach", FileEntry $ textRWFile (pure "") (\_ -> ensureWDevice ptr $ do
                    siblings <- getDeviceSiblings ptr
                    mapM_ detachDevice $ ptr: S.toList siblings
                    pure $ Right ()
                                                          )
              )
            , ("attach", FileEntry $ textRWFile (pure "") (\seat -> ensureWDevice ptr $ do
                    siblings <- getDeviceSiblings ptr
                    mapM_ (`attachDevice` seat) $ ptr: S.toList siblings
                    pure $ Right ()
                                                          )
              )
            ]
    let makeDevLink sib = do
            devName <- liftIO $ T.unpack <$> getDeviceName sib
            pure (devName, SymlinkEntry $ pure $ "../../" ++ devName)
    siblings <- getDeviceSiblings ptr
    let sibDir = if S.null siblings
        then mempty
        else [("siblings", DirEntry $ enumeratingDir $ fmap M.fromList $ mapM makeDevLink $ S.toList siblings)]

    name <- liftIO $ getDeviceName ptr

    pure (T.unpack name, DirEntry $ simpleDir $ M.fromList (deviceType ++ sibDir))


enumerateInputs :: (FocusCore vs a, WSTag a) => Way vs a (Map String (Entry vs a))
enumerateInputs = do
    inputRef <- inputDevices . compInput . wayCompositor <$> getState
    inputs <- liftIO $ readIORef inputRef
    M.fromList <$> mapM makeInputDir (S.toList inputs)


makeFooDir :: (Text, SeatFoo) -> Way vs a (String, Entry vs a)
makeFooDir (name, foo) = do
    devPtrs <- liftIO $ readIORef $ fooDevices foo
    let makeDevLink ptr = do
            devName <- liftIO $ T.unpack <$> getDeviceName ptr
            pure (devName, SymlinkEntry $ pure $ "../../../devices/" ++ devName)
    let devDir = ("devices", DirEntry $ enumeratingDir $ fmap M.fromList $ mapM makeDevLink $ S.toList devPtrs)

    pure (T.unpack name, DirEntry $ simpleDir $ M.fromList [devDir])

enumerateSeats :: Way vs a (Map String (Entry vs a))
enumerateSeats = do
    fooRef <- inputFooMap . compInput . wayCompositor <$> getState
    foos <- liftIO $ readIORef fooRef
    M.fromList <$> mapM makeFooDir (M.toList foos)

inputsDir :: (FocusCore vs a, WSTag a) => Entry vs a
inputsDir =
    DirEntry $ simpleDir $ M.fromList
        [ ("devices", DirEntry $ enumeratingDir enumerateInputs)
        , ("seats", DirEntry $ enumeratingDir enumerateSeats)
        ]
