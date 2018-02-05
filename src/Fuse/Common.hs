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
module Fuse.Common
where

import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Foreign.C.Error (Errno, eNOENT, eNOTSUP, ePERM, eINVAL)
import System.Posix.Types (ByteCount, FileOffset)
import System.Posix.Files

import System.Fuse

import Waymonad.Utility.Base (firstDir)
import Waymonad.Types (Way)

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E

defaultDirStats :: FuseContext -> FileStat
defaultDirStats ctx = FileStat
    { statEntryType = Directory
    , statFileMode = ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode
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

defaultFileStats :: FuseContext -> FileStat
defaultFileStats ctx = (defaultDirStats ctx) { statEntryType = RegularFile, statFileMode = ownerReadMode .|. ownerWriteMode }

defaultSymStats :: FuseContext -> FileStat
defaultSymStats ctx = (defaultDirStats ctx) { statEntryType = SymbolicLink }


data FileHandle vs a = FileHandle
    { fileRead :: FilePath -> ByteCount -> FileOffset -> Way vs a (Either Errno ByteString)
    , fileWrite :: FilePath -> ByteString -> FileOffset -> Way vs a (Either Errno ByteCount)
    , fileFlush :: FilePath -> Way vs a Errno
    , fileRelease :: FilePath -> Way vs a ()
    }

data DirHandle vs a = DirHandle
    { dirRead :: FilePath -> Way vs a (Either Errno [(FilePath, FileStat)])
    , dirOpenFile :: FilePath -> OpenMode -> OpenFileFlags -> Way vs a (Either Errno (FileHandle vs a))
    , dirReadSym :: FilePath -> Way vs a (Either Errno FilePath)
    , dirGetStat :: FilePath -> Way vs a (Either Errno FileStat)
    }

data Entry vs a
    = FileEntry (OpenMode, FileHandle vs a)
    | DirEntry (DirHandle vs a)
    | SymlinkEntry (Way vs a FilePath)


getDefaultStats :: Entry vs a -> FuseContext -> FileStat
getDefaultStats (FileEntry _) = defaultFileStats
getDefaultStats (DirEntry _) = defaultDirStats
getDefaultStats (SymlinkEntry _) = defaultSymStats


compatible :: OpenMode -> OpenMode -> Bool
compatible ReadOnly ReadOnly = True
compatible ReadOnly ReadWrite = True
compatible WriteOnly WriteOnly = True
compatible WriteOnly ReadWrite = True
compatible ReadWrite ReadWrite = True
compatible _ _ = False


simpleRead :: Map String (Entry vs a) -> FilePath -> Way vs a (Either Errno [(FilePath, FileStat)])
simpleRead m "/" = do
    ctx <- liftIO getFuseContext
    pure $ Right $ map (\(name, entry) -> (name, getDefaultStats entry ctx)) $ M.toList m
simpleRead m path =
    let (top, sub) = firstDir $ tail path
     in case M.lookup top m of
            Nothing -> pure $ Left eNOENT
            Just (DirEntry handle) -> case sub of
                [] -> dirRead handle "/"
                _ -> dirRead handle sub
            _ -> pure $ Left eNOTDIR


simpleOpenFile :: Map String (Entry vs a) -> FilePath -> OpenMode -> OpenFileFlags -> Way vs a (Either Errno (FileHandle vs a))
simpleOpenFile m path mode flags =
    let (top, sub) = firstDir $ tail path
     in case M.lookup top m of
            Nothing -> pure $ Left eNOENT
            Just (DirEntry entry) -> dirOpenFile entry sub mode flags
            Just (SymlinkEntry _) -> pure $ Left eNOTSUP
            Just (FileEntry (fMode, handle)) -> if mode `compatible` fMode
                then pure $ Right handle
                else pure $ Left ePERM


simpleReadSym :: Map String (Entry vs a) -> FilePath -> Way vs a (Either Errno FilePath)
simpleReadSym m path =
    let (top, sub) = firstDir $ tail path
     in case M.lookup top m of
            Nothing -> pure $ Left eNOENT
            Just (DirEntry handle) -> dirReadSym handle sub
            Just (SymlinkEntry act) -> Right <$> act
            _ -> pure $ Left eNOTSUP


simpleGetStat :: Map String (Entry vs a) -> FilePath -> Way vs a (Either Errno FileStat)
simpleGetStat _ "/" = do
    ctx <- liftIO getFuseContext
    pure $ Right $ defaultDirStats ctx
simpleGetStat m path = do
    ctx <- liftIO getFuseContext
    let (top, sub) = firstDir $ tail path
     in case M.lookup top m of
            Nothing -> pure $ Left eNOENT
            Just (DirEntry handle) -> case sub of
                [] -> pure $ Right $ defaultDirStats ctx
                _ -> dirGetStat handle sub
            Just x -> pure $ Right $ getDefaultStats x ctx


simpleDir :: Map FilePath (Entry vs a) -> DirHandle vs a
simpleDir m = DirHandle
    { dirRead = simpleRead m
    , dirOpenFile = simpleOpenFile m
    , dirReadSym = simpleReadSym m
    , dirGetStat = simpleGetStat m
    }


enumeratingDir :: Way vs a (Map String (Entry vs a)) -> DirHandle vs a
enumeratingDir act = DirHandle
    { dirRead = \path -> flip simpleRead path =<< act
    , dirOpenFile = \path mode flags -> (\m -> simpleOpenFile m path mode flags) =<< act
    , dirReadSym = \path -> flip simpleReadSym path =<< act
    , dirGetStat = \path -> flip simpleGetStat path =<< act
    }

bytestringFile :: Way vs a ByteString -> (OpenMode, FileHandle vs a)
bytestringFile bsGen = (ReadOnly,
    FileHandle
        { fileRead = \_ count offset -> do
            bs <- bsGen
            pure $ Right $ BS.take (fromIntegral count) $ BS.drop (fromIntegral offset) bs
        , fileWrite = \_ _ _ -> pure $ Left eNOTSUP
        , fileFlush = \_ -> pure eOK
        , fileRelease = \_ -> pure ()
        }
    )

bytestringRWFile
    :: Way vs a ByteString
    -> (ByteString -> Way vs a (Either Errno ()))
    -> (OpenMode, FileHandle vs a)
bytestringRWFile bsGen bsTake = (ReadWrite,
    FileHandle
        { fileRead = \_ count offset -> do
            bs <- bsGen
            pure $ Right $ BS.take (fromIntegral count) $ BS.drop (fromIntegral offset) bs
        , fileWrite = \_ bs _ -> do
            ret <- bsTake bs
            pure $ fmap (const $ fromIntegral $ BS.length bs) ret
        , fileFlush = \_ -> pure eOK
        , fileRelease = \_ -> pure ()
        }
    )


textFile :: Way vs a Text -> (OpenMode, FileHandle vs a)
textFile txtGen = bytestringFile (E.encodeUtf8 <$> txtGen)

textRWFile
    :: Way vs a Text
    -> (Text -> Way vs a (Either Errno ()))
    -> (OpenMode, FileHandle vs a)
textRWFile txtGen txtTake =
    bytestringRWFile
        (E.encodeUtf8 <$> txtGen)
        (\bs -> case E.decodeUtf8' bs of
            Left _ -> pure $ Left eINVAL
            Right x -> txtTake x
        )
