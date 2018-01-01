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

import Debug.Trace

import System.IO
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Foreign.C.Error (Errno, eOK, eNOENT)
import System.Fuse

import Utility (firstDir)
import ViewSet (WSTag (..))
import Waymonad (getSeat, getState, getLoggers, runWay, makeCallback)
import Waymonad.Types (Way)

import Fuse.Common
import Fuse.Outputs
import Fuse.Workspaces

import qualified Data.Map as M

openDir :: FilePath -> IO Errno
openDir _ = pure eOK

-- fileStat :: FilePath -> IO (Either Errno FileStat)
-- fileStat _ = do
--     ctx <- getFuseContext
--     pure $ Right $ defaultDirStats ctx
-- fileStat _ = pure $ Left eNOENT
-- 
-- readDir :: WSTag a => FilePath -> Way a (Either Errno [(FilePath, FileStat)])
-- readDir "/" = do
--     ctx <- liftIO $ getFuseContext
--     pure $ Right [("outputs", defaultDirStats ctx), ("workspaces", defaultDirStats ctx)]
-- readDir path = do
--     let (top, sub) = firstDir $ tail path
--      in case traceShowId top of
--             "outputs" -> readOutputs sub
--             "workspaces" -> readWorkspaces sub
--             _ -> pure $ Left eNOENT

fuseOps :: WSTag a => Way a (FuseOperations (FileHandle a))
fuseOps = do
    seat <- getSeat
    state <- getState
    loggers <- getLoggers

    let fileReadCB = \path (FileHandle {fileRead = fun}) bc off ->
            runWay seat state loggers $ fun path bc off
    let fileWriteCB = \path (FileHandle {fileWrite = fun}) bs off ->
            runWay seat state loggers $ fun path bs off

    dirReadCB <- makeCallback (dirRead mainDir)
    let openFileCB = \path mode flags ->
            runWay seat state loggers $ dirOpenFile mainDir path mode flags
    statCB <- makeCallback (dirGetStat mainDir)

    pure $ defaultFuseOps
        { fuseOpenDirectory = openDir
        , fuseReadDirectory = dirReadCB

        , fuseGetFileStat = statCB

        , fuseOpen = openFileCB
        , fuseRead = fileReadCB
        , fuseWrite = fileWriteCB
        }

mainDir :: WSTag a => DirHandle a
mainDir = simpleDir $ M.fromList [("workspaces", workspaceDir)]

runFuse :: WSTag a => Way a ()
runFuse = do
    ops <- fuseOps
    liftIO $ void  $ forkIO $ fuseRunInline "waymonad" ["/var/run/waymonad"] ops defaultExceptionHandler
