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
module Fuse.Outputs
    ( readOutputs
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Foreign.C.Error (Errno, eOK, eNOENT)
import System.Fuse

import Output (Output(outputName))
import Utility (firstDir)
import Waymonad.Types (Way)
import WayUtil (getOutputs)

import qualified Data.Text as T

defaultFileStats :: FuseContext -> FileStat
defaultFileStats ctx = FileStat
    { statEntryType = RegularFile
    , statFileMode = 0444
    , statLinkCount = 1
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = 4096
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

defaultDirStats :: FuseContext -> FileStat
defaultDirStats ctx = FileStat
    { statEntryType = Directory
    , statFileMode = 0444
    , statLinkCount = 1
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = 4096
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }


readOutput :: FilePath -> Output -> Way a (Either Errno [(FilePath, FileStat)])
readOutput "" output = do
    ctx <- liftIO $ getFuseContext
    pure $ Right [("modes", defaultFileStats ctx), ("mode", defaultFileStats ctx)]
readOutput _ _ = pure $ Left eNOENT

readOutputs :: FilePath -> Way a (Either Errno [(FilePath, FileStat)])
readOutputs "" = do
    ctx <- liftIO $ getFuseContext
    outputs <- getOutputs
    pure $ Right $ fmap (\out -> (T.unpack $ outputName out, defaultDirStats ctx)) outputs
readOutputs path = do
    let (top, sub) = firstDir $ tail path
        name = T.pack top
    outputs <- getOutputs
    case find ((==) name . outputName) outputs of
        Nothing -> pure $ Left $ eNOENT
        Just out -> readOutput sub out
