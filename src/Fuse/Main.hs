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
{-# LANGUAGE ScopedTypeVariables #-}
module Fuse.Main
where

import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.Fuse

import Graphics.Wayland.Server
    ( DisplayServer
    , displayGetEventLoop
    , eventLoopAddFd
    , clientStateReadable
    , eventSourceRemove
    )

import Shared (Bracketed (..))
import ViewSet (WSTag, FocusCore)
import WayUtil (closeCompositor)
import Waymonad (getSeat, getState, getLoggers, runWay, makeCallback, unliftWay)
import Waymonad.Types (Way)

import Fuse.Common
import Fuse.Inputs
import Fuse.Outputs
import Fuse.Workspaces


import qualified Data.Map as M

openDir :: FilePath -> IO Errno
openDir _ = pure eOK

closeFile :: Entry vs a
closeFile = FileEntry $ bytestringRWFile
    (pure mempty)
    (\_ -> Right <$> closeCompositor)

fuseOps :: DirHandle vs a -> Way vs a (FuseOperations (FileHandle vs a))
fuseOps dir = do
    seat <- getSeat
    state <- getState
    loggers <- getLoggers

    let fileReadCB path FileHandle {fileRead = fun} bc off =
            runWay seat state loggers $ fun path bc off
    let fileWriteCB path FileHandle {fileWrite = fun} bs off =
            runWay seat state loggers $ fun path bs off
    let fileFlushCB path file = runWay seat state loggers $ fileFlush file path
    let fileReleaseCB path file = runWay seat state loggers $ fileRelease file path
    let fileSetSizeCB _ _ = runWay seat state loggers $ pure eOK

    dirReadCB <- makeCallback (dirRead dir)
    let openFileCB path mode flags = runWay seat state loggers $ dirOpenFile dir path mode flags
    statCB <- makeCallback (dirGetStat dir)

    readLinkCB <- makeCallback (dirReadSym dir)

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

mainDir :: (FocusCore vs a, WSTag a) => DirHandle vs a
mainDir = simpleDir $ M.fromList
    [ ("workspaces", workspaceDir)
    , ("outputs", outputsDir)
    , ("shutdown", closeFile)
    , ("inputs", inputsDir)
    ]


getFuseBracket :: (FocusCore vs a, WSTag a) => Bracketed vs DisplayServer a
getFuseBracket = do


    PreBracket (\dsp act -> do
        ops <- fuseOps mainDir
        runtimeDir <- liftIO $ getEnv "XDG_RUNTIME_DIR"
        let fuseDir = runtimeDir ++ "/waymonad"
        liftIO $ createDirectoryIfMissing False fuseDir
        evtLoop <- liftIO $ displayGetEventLoop dsp
        let register fd cb = eventLoopAddFd evtLoop fd clientStateReadable (\ _ _ -> cb >> pure False)
        pass <- unliftWay act
        liftIO $ fuseRunInline
            register
            eventSourceRemove
            pass
            "waymonad"
            [fuseDir, "-o", "default_permissions,auto_unmount"]
            ops
            defaultExceptionHandler
                )
