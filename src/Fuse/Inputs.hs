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
{-# LANGUAGE TupleSections #-}
module Fuse.Inputs
    ( inputsDir
    )
where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Map (Map)
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import Foreign.C.Error (Errno, eBADF, eINVAL)
import Fuse.Common

import Graphics.Wayland.WlRoots.Backend.Libinput (getDeviceHandle)
import Graphics.Wayland.WlRoots.Input (InputDevice, getDeviceName, inputDeviceType)


import Waymonad.Input
import Waymonad.Input.Libinput
import Waymonad.Utility.Base (doJust)
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad
import Waymonad.Types (Compositor (compInput), Output (outputName))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified System.InputDevice as LI

import System.IO

ensureWDevice :: Ptr InputDevice -> Way vs ws (Either Errno a) -> Way vs ws (Either Errno a)
ensureWDevice ptr act = do
    devs <- liftIO . readIORef . inputDevices . compInput . wayCompositor =<< getState
    if ptr `S.member` devs
        then act
        else do
            liftIO $ hPutStrLn stderr "Couldn't ensure a device"
            pure $ Left eBADF

makeOptionDir :: LI.InputDevice -> LibinputOption -> (String, Entry vs ws)
makeOptionDir dev opt =
    let guaranteed =
            [ ("default", FileEntry $ textFile . liftIO $ optionDefault opt dev)
            , ("value", FileEntry $ textRWFile
                    (liftIO $ optionGet opt dev)
                    (\txt -> liftIO $ do
                        ret <- optionSet opt dev txt
                        case ret of
                            Nothing -> pure $ Right ()
                            Just err -> liftIO $ do
                                hPutStrLn stderr "Failed to set option:"
                                hPutStrLn stderr (T.unpack err)
                                pure $ Left eINVAL
                    )
              )
            ]
        valids = maybe id (\fun -> (:) ("valid", FileEntry $ textFile . liftIO $ fun dev)) (optionValids opt)
        files = valids guaranteed
     in  (T.unpack $ optionName opt, DirEntry $ simpleDir $ M.fromList files)

makeOptionsDir :: Ptr InputDevice -> Way vs ws (Maybe (Entry vs ws))
makeOptionsDir dev = liftIO $ doJust (getDeviceHandle dev) $ \handle -> do
    opts <- filterM (\opt -> optionExists opt handle) libinputOptions
    if null opts
        then pure Nothing
        else pure . Just $ DirEntry . simpleDir . M.fromList $ map (makeOptionDir handle) opts


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
            devName <- liftIO $ filter (/= '/') . T.unpack <$> getDeviceName sib
            pure (devName, SymlinkEntry $ pure $ "../../" ++ devName)
    siblings <- getDeviceSiblings ptr
    let sibDir = if S.null siblings
        then mempty
        else [("siblings", DirEntry $ enumeratingDir $ fmap M.fromList $ mapM makeDevLink $ S.toList siblings)]

    name <- liftIO $ getDeviceName ptr
    optDir <- makeOptionsDir ptr
    let optFun = maybe id (:) $ fmap ("options",) optDir

    liData <- doJust (liftIO $ getDeviceHandle ptr) $ \handle ->
        pure
            [ ("vendor", FileEntry $ textFile . fmap (T.pack . show) . liftIO $ LI.getDeviceVendor handle)
            , ("product", FileEntry $ textFile . fmap (T.pack . show) . liftIO $ LI.getDeviceProduct handle)
            , ("sysname", FileEntry $ textFile . liftIO $ LI.getDeviceSysname handle)
            ]


    pure (filter (/= '/') $ T.unpack name, DirEntry $ simpleDir $ M.fromList (optFun deviceType ++ sibDir ++ liData))


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

    currents <- liftIO . readIORef . wayBindingCurrent =<< getState
    let focus = case lookup (fooSeat foo) currents of
            Nothing -> []
            Just (p, k) ->
                [ ("pointer", SymlinkEntry $ pure $ "../../../outputs/" ++ (T.unpack $ outputName p))
                , ("keyboard", SymlinkEntry $ pure $ "../../../outputs/" ++(T.unpack $ outputName k))
                ]

    pure (T.unpack name, DirEntry $ simpleDir $ M.fromList $ devDir: focus)

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
