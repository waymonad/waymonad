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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Fuse.Workspaces
    ( workspaceDir
    )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Maybe (fromMaybe)

import Waymonad.View (getViewTitle, getViewAppId)
import Waymonad.ViewSet (WSTag (..){-, LayoutClass (..), GenericLayout (..)-}, FocusCore)
import Waymonad.Types (Way, Output (outputName))
import Waymonad.Utility.Focus (getWorkspaceOutputs)
import Waymonad.Utility.ViewSet (getWorkspaces, getWorkspaceViews)

import Fuse.Common

import qualified Data.Map as M
import qualified Data.Text as T

makeViewDir :: (FocusCore vs a, WSTag a) => a -> Way vs a (Maybe (Entry vs a))
makeViewDir ws = do
    views <- getWorkspaceViews ws
    case views of
        [] -> pure Nothing
        _ -> Just <$> do
                tmp <- forM views $ \view -> liftIO $ do
                    title <-fromMaybe "<None>" <$> getViewTitle view
                    appId <- fromMaybe "<None>" <$> getViewAppId view
                    pure $ title `T.append` " : " `T.append` appId

                let content = T.intercalate "\n" tmp
                pure $ FileEntry . textFile $ pure content

makeOutputDir :: (FocusCore vs a, WSTag a) => a -> Way vs a (Maybe (Entry vs a))
makeOutputDir ws = do
    outs <- getWorkspaceOutputs ws
    case outs of
        [] -> pure Nothing
        xs -> pure . Just . DirEntry . simpleDir . M.fromList . flip fmap xs $ \out ->
                let name = T.unpack $ outputName out
                 in (name, SymlinkEntry . pure $ "../../../outputs/" ++ name)

makeWorkspaceDir :: (FocusCore vs a, WSTag a) => a -> Way vs a (Entry vs a)
makeWorkspaceDir ws = do
    let layout =
            [{- ("layout", FileEntry $ textFile $  do
                    Workspace (GenericLayout l) _ <- getWorkspace ws
                    pure $ description l)
            , ("current", FileEntry $ textFile $  do
                    Workspace (GenericLayout l) _ <- getWorkspace ws
                    pure $ currentDesc l)-}
            ]

    outs <- makeOutputDir ws
    let outDir = maybe id ((:) . ("outputs",)) outs

    views <- makeViewDir ws
    let viewDir = maybe id ((:) . ("views", )) views

    pure $ DirEntry $ simpleDir $ M.fromList $ viewDir . outDir $ layout

enumerateWSS :: (FocusCore vs a, WSTag a) => Way vs a (Map String (Entry vs a))
enumerateWSS = do
    wss <- getWorkspaces
    M.fromList <$> mapM (\ws -> (T.unpack $ getName ws,) <$> makeWorkspaceDir ws) wss

workspaceDir :: (FocusCore vs a, WSTag a) => Entry vs a
workspaceDir = DirEntry $ enumeratingDir enumerateWSS
