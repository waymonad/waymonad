{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Utility.Spawn
    ( spawn
    , spawnNamed
    , getClientName
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IntMap (IntMap)
import Data.Text (Text)
import Network.Socket
    ( Socket
    , socketPair
    , Family(AF_UNIX)
    , SocketType(Stream)
    , fdSocket
    , close
    )
import System.Posix.Env (putEnv)
import System.Posix.Types (Fd (..))
import System.Posix.Process (forkProcess, executeFile)
import System.Process (spawnCommand)

import Graphics.Wayland.Server (Client (..), clientCreate, DisplayServer)
import Graphics.Wayland.Server.Client (addDestroyListener)

import Utility (whenJust, ptrToInt)
import Waymonad (Way, setCallback, WayLoggers(loggerSpawner))
import Waymonad.Extensible (ExtensionClass (..))
import WayUtil
    ( logPutText
    , getEState
    , modifyEState
    )

import qualified Data.IntMap.Strict as IM
import Data.Text as T

-- The most simple generic spawn function
spawn :: (MonadIO m) => String -> m ()
spawn = void . liftIO . spawnCommand

newtype NameMap = NameMap { unNM :: (IntMap Text) }
    deriving (Show, Eq, Monoid)

instance ExtensionClass NameMap where
    initialValue = mempty

spawnClient :: Socket -> Fd -> String -> [String] -> IO ()
spawnClient socket fd cmd args = do
    close socket
    putEnv $ "WAYLAND_SOCKET=" ++ show fd
    executeFile cmd True args Nothing

clientToInt :: Client -> Int
clientToInt (Client c) = ptrToInt c

spawnNamed
    :: DisplayServer
    -> Text
    -> String
    -> [String]
    -> Way a ()
spawnNamed dsp name cmd args = do
    (cSock, sSock) <- liftIO $ socketPair AF_UNIX Stream 0
    void . liftIO . forkProcess $ spawnClient sSock (Fd $ fdSocket cSock) cmd args

    liftIO $ close cSock
    let cFd = fdSocket sSock
    clientM <- liftIO $ clientCreate dsp (Fd cFd)
    whenJust clientM $ \client -> do
        logPutText loggerSpawner $ "Going to spawn: " `T.append` name
        modifyEState (NameMap . IM.insert (clientToInt client) name . unNM)
        setCallback (\_ -> modifyEState $ NameMap . IM.delete (clientToInt client) . unNM) (addDestroyListener client)

getClientName :: Client -> Way a (Maybe (Text))
getClientName c = IM.lookup (clientToInt c) . unNM <$> getEState
