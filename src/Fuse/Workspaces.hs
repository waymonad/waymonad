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
module Fuse.Workspaces
    ( workspaceDir
    )
where

import Debug.Trace

import System.IO
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Foreign.C.Error (Errno, eOK, eNOENT, ePERM, eNOTSUP)
import System.Fuse

import Utility (firstDir)
import ViewSet (WSTag (..), Workspace (..), LayoutClass (..), Layout (..))
import Waymonad (makeCallback, getWorkspace)
import Waymonad.Types (Way)
import WayUtil.ViewSet (getWorkspaces)

import Fuse.Outputs
import Fuse.Common

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS



readWorkspace :: WSTag a => FilePath -> a -> Way a (Either Errno [(FilePath, FileStat)])
readWorkspace "" _ = do
    ctx <- liftIO $ getFuseContext
    pure $ Right
        [ ("layout", defaultFileStats ctx)
        , ("current", defaultFileStats ctx)
        ]
readWorkspace _ _ = pure $ Left $ eNOENT

readWorkspaces :: WSTag a => FilePath -> Way a (Either Errno [(FilePath, FileStat)])
readWorkspaces "" = do
    ctx <- liftIO $ getFuseContext
    wss <- getWorkspaces
    pure $ Right $ fmap (\ws -> (T.unpack $ getName ws, defaultDirStats ctx)) wss
readWorkspaces path = do
    let (top, sub) = firstDir $ tail path
        name = T.pack top
    wss <- getWorkspaces
    case find ((==) name . getName) wss of
        Just ws -> readWorkspace sub ws
        Nothing -> pure $ Left $ eNOENT

openWSFile :: WSTag a => FilePath -> a -> OpenMode -> OpenFileFlags -> Way a (Either Errno (FileHandle a))
openWSFile _ _ WriteOnly _ = pure $ Left ePERM
openWSFile _ _ ReadWrite _ = pure $ Left ePERM
openWSFile "/layout" ws _ _ = pure $ Right FileHandle
    { fileRead = \_ count offset -> do
        Workspace (Layout l) _ <- getWorkspace ws
        let desc = description l
            bs = E.encodeUtf8 desc
            ret = BS.take (fromIntegral count) $ BS.drop (fromIntegral offset) bs
        pure $ Right ret
    , fileWrite = \_ _ _ -> pure $ Left eNOTSUP
    , fileFlush = \_ -> pure eOK
    , fileRelease = \_ -> pure ()
    }
openWSFile "/current" ws _ _ = pure $ Right FileHandle
    { fileRead = \_ count offset -> do
        Workspace (Layout l) _ <- getWorkspace ws
        let desc = currentDesc l
            bs = E.encodeUtf8 desc
            ret = BS.take (fromIntegral count) $ BS.drop (fromIntegral offset) bs
        pure $ Right ret
    , fileWrite = \_ _ _ -> pure $ Left eNOTSUP
    , fileFlush = \_ -> pure eOK
    , fileRelease = \_ -> pure ()
    }
openWSFile _ _ _ _ = pure $ Left eNOENT

openWSSFile :: WSTag a => FilePath -> OpenMode -> OpenFileFlags -> Way a (Either Errno (FileHandle a))
openWSSFile path mode flags = do
    let (top, sub) = firstDir $ tail path
        name = T.pack top
    wss <- getWorkspaces
    case find ((==) name . getName) wss of
        Just ws -> openWSFile sub ws mode flags
        Nothing -> pure $ Left $ eNOENT

wsGetStat :: FilePath -> a -> Way a (Either Errno FileStat)
wsGetStat "/layout" _ = do
    ctx <- liftIO $ getFuseContext
    pure $ Right $ defaultFileStats ctx
wsGetStat "/current" _ = do
    ctx <- liftIO $ getFuseContext
    pure $ Right $ defaultFileStats ctx
wsGetStat _ _ = pure $ Left $ eNOENT

wssGetStat :: WSTag a => FilePath -> Way a (Either Errno FileStat)
wssGetStat path = do
    let (top, sub) = firstDir $ tail path
        name = T.pack top
    wss <- getWorkspaces
    case find ((==) name . getName) wss of
        Just ws -> case sub of
            [] -> do
                ctx <- liftIO $ getFuseContext
                pure $ Right $ defaultDirStats ctx
            _ -> wsGetStat sub ws
        Nothing -> pure $ Left $ eNOENT

workspaceDir :: WSTag a => Entry a
workspaceDir = DirEntry $ DirHandle
    { dirRead = readWorkspaces
    , dirOpenFile = openWSSFile
    , dirReadSym = const $ pure $ Left eNOENT
    , dirGetStat = wssGetStat
    }
