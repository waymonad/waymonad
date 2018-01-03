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
module Fuse.Main
where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.Fuse

import ViewSet (WSTag (..))
import Waymonad (getSeat, getState, getLoggers, runWay, makeCallback)
import Waymonad.Types (Way)

import Fuse.Common
import Fuse.Outputs
import Fuse.Workspaces

import qualified Data.Map as M

openDir :: FilePath -> IO Errno
openDir _ = pure eOK

fuseOps :: WSTag a => Way a (FuseOperations (FileHandle a))
fuseOps = do
    seat <- getSeat
    state <- getState
    loggers <- getLoggers

    let fileReadCB = \path (FileHandle {fileRead = fun}) bc off ->
            runWay seat state loggers $ fun path bc off
    let fileWriteCB = \path (FileHandle {fileWrite = fun}) bs off ->
            runWay seat state loggers $ fun path bs off
    let fileFlushCB = \path file ->
            runWay seat state loggers $ fileFlush file path
    let fileReleaseCB = \path file ->
            runWay seat state loggers $ fileRelease file path
    let fileSetSizeCB = \_ _ ->
            runWay seat state loggers $ pure eOK

    dirReadCB <- makeCallback (dirRead mainDir)
    let openFileCB = \path mode flags ->
            runWay seat state loggers $ dirOpenFile mainDir path mode flags
    statCB <- makeCallback (dirGetStat mainDir)

    readLinkCB <- makeCallback (dirReadSym mainDir)

    pure $ defaultFuseOps
        { fuseOpenDirectory = openDir
        , fuseReadDirectory = dirReadCB
        , fuseReadSymbolicLink = readLinkCB

        , fuseGetFileStat = statCB

        , fuseOpen = openFileCB
        , fuseRead = fileReadCB
        , fuseWrite = fileWriteCB
        , fuseFlush = fileFlushCB
        , fuseRelease = fileReleaseCB
        , fuseSetFileSize = fileSetSizeCB
        }

mainDir :: WSTag a => DirHandle a
mainDir = simpleDir $ M.fromList
    [ ("workspaces", workspaceDir)
    , ("outputs", outputsDir)
    ]

runFuse :: WSTag a => Way a ()
runFuse = do
    ops <- fuseOps
    runtimeDir <- liftIO $ getEnv "XDG_RUNTIME_DIR"
    let fuseDir = runtimeDir ++ "/waymonad"
    liftIO $ createDirectoryIfMissing False fuseDir
    liftIO $ void  $ forkIO $ fuseRunInline "waymonad" [fuseDir, "-o", "default_permissions"] ops defaultExceptionHandler
