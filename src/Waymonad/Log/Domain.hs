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
module Waymonad.Log.Domain
    ( logFun
    )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (catch, throw)
import Control.Monad (forever, foldM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.IO.Error (isDoesNotExistError, IOError)

import Network.Socket
    ( Socket
    , accept
    , Family(AF_UNIX)
    , SocketType(Stream)
    , SockAddr(SockAddrUnix)
    , SocketOption(ReuseAddr)
    , socket
    , bind
    , listen
    , setSocketOption
    )
import Network.Socket.ByteString (sendAll)

import Waymonad.View (getViewTitle)
import Waymonad.ViewSet (WSTag (..))
import Waymonad
import Waymonad.Utility.Current (getCurrentView)

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

hLogFun :: WSTag a => (Text -> IO ()) -> Way vs a ()
hLogFun prnt = do
    allWS <- wayUserWorkspaces <$> getState
    actives <- fmap (map fst) . liftIO . readIORef . wayBindingMapping =<< getState

    let tagged = map (\ws -> (ws `elem` actives, ws)) allWS

    let workspaces = T.concat $ fmap (\(active, ws) ->
            "| "
            `T.append` (if active then "<" else "")
            `T.append` getName ws
            `T.append` (if active then "> " else " ")
            ) tagged
    view <- getCurrentView
    out <- case view of
        Nothing -> pure workspaces
        Just v -> do
            title <- fromMaybe "<No Title>" <$> getViewTitle v
            pure $ workspaces `T.append` " : " `T.append` title
    liftIO $ prnt $ out `T.append` "\n"


getRuntimeDir :: IO String
getRuntimeDir = do
    xdg <- lookupEnv "XDG_RUNTIME_DIR"
    case xdg of
        Just x -> pure x
        Nothing -> getTemporaryDirectory

getSocket :: IO Socket
getSocket = do
    dir <- getRuntimeDir
    catch (removeFile $ dir ++ "/waymonad_log") $ \e ->
        if isDoesNotExistError e
            then pure ()
            else throw e
    server <- socket AF_UNIX Stream 0
    setSocketOption server ReuseAddr 1 
    bind server $ SockAddrUnix $ dir ++ "/waymonad_log"
    listen server 5
    pure server

acceptLoop :: Socket -> MVar [Socket] -> IO ()
acceptLoop server clients = do
    (new, _) <- accept server
    modifyMVar_ clients (pure . (:) new)

printSocket :: ByteString -> [Socket] -> Socket -> IO [Socket]
printSocket bs xs s =
    good `catch` (\(_ :: IOError) -> pure xs)
    where good = sendAll s bs >> pure (s:xs)

printFun :: MVar [Socket] -> Text -> IO ()
printFun clients text = do
    let bs = E.encodeUtf8 text
    modifyMVar_ clients $ foldM (printSocket bs) []


logFun :: WSTag a => IO (Way vs a ())
logFun = do
    server <- getSocket
    clients <- newMVar []
    _ <- forkIO $ forever $ acceptLoop server clients
    pure . hLogFun $ printFun clients
