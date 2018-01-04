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
module Fuse.Workspaces
    ( workspaceDir
    )
where

import Data.Map (Map)

import Output (Output (outputName))
import ViewSet (WSTag (..), Workspace (..), LayoutClass (..), Layout (..))
import Waymonad (getWorkspace)
import Waymonad.Types (Way)
import WayUtil.Focus (getWorkspaceOutputs)
import WayUtil.ViewSet (getWorkspaces)

import Fuse.Common

import qualified Data.Map as M
import qualified Data.Text as T


makeOutputDir :: WSTag a => a -> Way a (Maybe (Entry a))
makeOutputDir ws = do
    outs <- getWorkspaceOutputs ws
    case outs of
        [] -> pure Nothing
        xs -> pure . Just . DirEntry . simpleDir . M.fromList . flip fmap xs $ \out ->
                let name = T.unpack $ outputName out
                 in (name, SymlinkEntry . pure $ "../../../outputs/" ++ name)

makeWorkspaceDir :: WSTag a => a -> Way a (Entry a)
makeWorkspaceDir ws = do
    let layout =
            [ ("layout", FileEntry $ textFile $  do
                    Workspace (Layout l) _ <- getWorkspace ws
                    pure $ description l)
            , ("current", FileEntry $ textFile $  do
                    Workspace (Layout l) _ <- getWorkspace ws
                    pure $ currentDesc l)
            ]
    outs <- makeOutputDir ws
    let outDir = maybe [] (pure . ("outputs",)) outs

    pure $ DirEntry $ simpleDir $ M.fromList $ outDir ++ layout

enumerateWSS :: WSTag a => Way a (Map String (Entry a))
enumerateWSS = do
    wss <- getWorkspaces
    M.fromList <$> mapM (\ws -> (T.unpack $ getName ws,) <$> makeWorkspaceDir ws) wss

workspaceDir :: WSTag a => Entry a
workspaceDir = DirEntry $ enumeratingDir enumerateWSS
