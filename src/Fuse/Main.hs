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

import System.IO

import Control.Monad.IO.Class (liftIO)
import Data.Typeable (Typeable)
import System.Directory (createDirectoryIfMissing, removeDirectory)
import System.Environment (getEnv)
import System.Fuse

import Graphics.Wayland.Server
    ( DisplayServer
    , displayGetEventLoop
    , eventLoopAddFd
    , clientStateReadable
    , eventSourceRemove
    )

import Waymonad.Start (Bracketed (..))
import Waymonad.ViewSet (WSTag, FocusCore)
import Waymonad.Utility (closeCompositor)
import Waymonad (getState, runWay, makeCallback, unliftWay)
import Waymonad.Types (Way)
import Waymonad.IPC

import Fuse.Common
import Fuse.Extensible
import Fuse.Inputs
import Fuse.Outputs
import Fuse.Rts
import Fuse.Shells
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
    state <- getState

    let fileReadCB path FileHandle {fileRead = fun} bc off =
            runWay state $ fun path bc off
    let fileWriteCB path FileHandle {fileWrite = fun} bs off =
            runWay state $ fun path bs off
    let fileFlushCB path file = runWay state $ fileFlush file path
    let fileReleaseCB path file = runWay state $ fileRelease file path
    let fileSetSizeCB _ _ = runWay state $ pure eOK

    dirReadCB <- makeCallback (dirRead dir)
    let openFileCB path mode flags = runWay state $ dirOpenFile dir path mode flags
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

mainDir :: (FocusCore vs ws, Typeable vs, WSTag ws) => IPCGroup vs ws -> DirHandle vs ws
mainDir grp = simpleDir $ M.fromList
    [ ("workspaces", workspaceDir)
    , ("outputs", outputsDir)
    , ("shutdown", closeFile)
    , ("inputs", inputsDir)
    , ("shells", shellsDir)
    , ("rts", rtsDir)
    , ("extensions", extensibleDir grp)
    ]


getFuseBracket :: (FocusCore vs ws, Typeable vs, WSTag ws) => IPCGroup vs ws -> Bracketed vs DisplayServer ws
getFuseBracket grp = PreBracket (\dsp act -> do
    ops <- fuseOps $ mainDir grp
    runtimeDir <- liftIO $ getEnv "XDG_RUNTIME_DIR"
    localDisplay <- liftIO $ getEnv "_WAYLAND_DISPLAY"
    let fuseDir = runtimeDir ++ "/waymonad/" ++ localDisplay
    liftIO $ createDirectoryIfMissing True fuseDir
    evtLoop <- liftIO $ displayGetEventLoop dsp
    let register fd cb = eventLoopAddFd evtLoop fd clientStateReadable (\ _ _ -> cb >> pure False)
    pass <- unliftWay act
    ret <- liftIO $ fuseRunInline
        register
        eventSourceRemove
        (\val -> do
            case val of
                Left err -> hPutStrLn stderr "Failed to start fuse:" >> hPutStrLn stderr err
                Right _ -> pure ()
            pass
        )
        "waymonad"
        [fuseDir, "-o", "default_permissions,auto_unmount"]
        ops
        defaultExceptionHandler
    liftIO $ removeDirectory fuseDir
    pure ret
                            )
