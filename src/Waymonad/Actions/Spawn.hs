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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Utility.Spawn
Description : Utility functions for spawning clients
Maintainer  : ongy
Stability   : testing
Portability : Linux

WARNING: The interesting things here only work with wayland clients.
XWayland clients won't be detected properly.
-}
module Waymonad.Actions.Spawn
    ( spawn

    , spawnNamed
    , manageNamed
    , namedSpawner
    , getClientName

    , spawnOn
    , manageSpawnOn
    , onSpawner

    , Spawner (..)
    , spawnManaged
    , spawnManaged'
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IntMap (IntMap)
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.Socket
    ( Socket
    , socketPair
    , Family(AF_UNIX)
    , SocketType(Stream)
    , fdSocket
    , close
    )
import System.Environment (setEnv)
import System.Posix.Types (Fd (..))
import System.Posix.Process (forkProcess, executeFile)
import System.Process (spawnCommand)

import Graphics.Wayland.Server (Client (..), clientCreate)
import Graphics.Wayland.Server.Client (addDestroyListener)

import Waymonad (Way, setCallback, WayLoggers(loggerSpawner))
import Waymonad.Extensible (ExtensionClass (..))
import Waymonad.Managehook (Managehook, query, liftWay, InsertAction (InsertInto))
import Waymonad.Utility (getDisplay)
import Waymonad.Utility.Base (whenJust, ptrToInt, doJust)
import Waymonad.Utility.Extensible (getEState , modifyEState)
import Waymonad.Utility.Log (logPutText, LogPriority (..))
import Waymonad.View (getViewClient)
import Waymonad.ViewSet (WSTag)

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

-- | The most simple generic spawn function
spawn :: (MonadIO m) => String -> m ()
spawn = void . liftIO . spawnCommand

newtype WSMap a = WSMap { unWM :: IntMap a }
    deriving (Show, Eq, Semigroup, Monoid)

instance Typeable a => ExtensionClass (WSMap a) where
    initialValue = mempty

newtype NameMap = NameMap { unNM :: IntMap Text }
    deriving (Show, Eq, Semigroup, Monoid)

instance ExtensionClass NameMap where
    initialValue = mempty

-- | Execute in the spawned process to exec the client
spawnClient :: Socket -> Fd -> String -> [String] -> IO () -> IO ()
spawnClient socket fd cmd args act = do
    close socket
    setEnv "WAYLAND_SOCKET" $ show fd
    act
    executeFile cmd True args Nothing

-- | Convert a client to an Int for storage in IntMap. Guaranteed to be unique
-- while the 'Client' is valid
clientToInt :: Client -> Int
clientToInt (Client c) = ptrToInt c

-- | Attach a name to a client. This name could be used by filters.
spawnNamed :: Text -> String -> [String] -> Way vs a ()
spawnNamed = spawnManaged . pure . namedSpawner

-- | 'Spawner' implementation for 'spawnNamed'
namedSpawner :: Text -> Spawner vs a
namedSpawner name = Spawner
    { spawnAdd = \client -> modifyEState $ NameMap . IM.insert (clientToInt client) name . unNM
    , spawnRm = \client -> modifyEState $ NameMap . IM.delete (clientToInt client) . unNM
    }

-- | Get the name given to a client with spawnNamed
getClientName :: Client -> Way vs a (Maybe Text)
getClientName c = IM.lookup (clientToInt c) . unNM <$> getEState

-- | Simple Managehook to log named spawns. Intended for debugging
manageNamed :: Managehook vs a
manageNamed = doJust (getViewClient =<< query) $ \c -> do
    liftWay $ do
        nameM <- getClientName c
        whenJust nameM $ \name ->
            logPutText loggerSpawner Info $ "Client \"" `T.append` name `T.append` "\" just spawned"
    mempty

-- | Spawn a client on the given workspace
spawnOn :: WSTag a => a -> String -> [String] -> Way vs a ()
spawnOn = spawnManaged . pure . onSpawner

-- | Get the workspace assigned to a client
getClientWS :: Typeable a => Client -> Way vs a (Maybe a)
getClientWS c = IM.lookup (clientToInt c) . unWM <$> getEState

-- | Managehook that checks whether a client had a Workspace attached and will
-- redirect it towards that if it does.
manageSpawnOn :: WSTag a => Managehook vs a
manageSpawnOn = doJust (getViewClient =<< query) $ \c -> do
    wsM <- liftWay $ getClientWS c
    case wsM of
        Nothing -> mempty
        Just ws -> pure $ InsertInto ws

-- | 'Spawner' implementation for 'spawnOn'
onSpawner :: forall vs a. WSTag a => a -> Spawner vs a
onSpawner ws = Spawner
    { spawnAdd = \client -> modifyEState $ WSMap . IM.insert (clientToInt client) ws . unWM
    , spawnRm = \client -> modifyEState $
        (WSMap :: IntMap a -> WSMap a) . IM.delete (clientToInt client) . unWM
    }

-- | A way to stack multiple data attachements on a client that should be
-- spanwed
data Spawner vs a = Spawner
    { spawnAdd :: Client -> Way vs a () -- ^Set up the client data
    , spawnRm :: Client -> Way vs a () -- ^Remove the client data. Will be called when the 'Client' object is destroyed.
    }

-- | Sapwn a client with multiple 'Spawner's attached, to e.g. place it on a
-- desired workspace and log it.
spawnManaged :: [Spawner vs a] -- ^The spawners to use
             -> String -- ^The application name
             -> [String] -- ^The application arguments
             -> Way vs a ()
spawnManaged spawner name args = spawnManaged' spawner name args (pure ())


spawnManaged' :: forall vs a.
                 [Spawner vs a] -- ^The spawners to use
              -> String -- ^The application name
              -> [String] -- ^The application arguments
              -> IO () -- ^An IO action to execute in the client process before execute
              -> Way vs a ()
spawnManaged' spawners cmd args act = do
    (cSock, sSock) <- liftIO $ socketPair AF_UNIX Stream 0
    cFd <- liftIO $ fdSocket cSock
    void . liftIO . forkProcess $ spawnClient sSock (Fd cFd) cmd args act

    liftIO $ close cSock
    sFd <- liftIO $ fdSocket sSock
    dsp <- getDisplay
    clientM <- liftIO $ clientCreate dsp (Fd sFd)
    whenJust clientM $ \client -> do
        addFun client
        setCallback rmFun (addDestroyListener client)
    where   addFun :: Client -> Way vs a ()
            addFun client = void . traverse ($ client) $ map spawnAdd spawners
            rmFun :: Client -> Way vs a ()
            rmFun client = void . traverse ($ client) $ map spawnRm spawners
