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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Fuse.Shells
where

import Control.Monad (forM)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Foreign.C.Error (eINVAL)

import Fuse.Common
import Waymonad.View (getViewTitle, getViewAppId)
import Waymonad.ViewSet (FocusCore, WSTag)
import Waymonad
import Waymonad.Types
import Waymonad.Shells

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

makeViewFile :: WayShell vs a -> Way vs a (Entry vs a)
makeViewFile shell = do
    views <- shellViews shell
    tmp <- forM (S.toList views) $ \view -> do
        title <-fromMaybe "<None>" <$> getViewTitle view
        appId <- fromMaybe "<None>" <$> getViewAppId view
        pure $ title `T.append` " : " `T.append` appId

    let content = T.intercalate "\n" tmp
    pure $ FileEntry . textFile $ pure content

makeShellDir :: (FocusCore vs a, WSTag a) => WayShell vs a -> Way vs a (Entry vs a)
makeShellDir shell = do
    let active = ("state",  FileEntry $ textRWFile
                    (T.pack . show <$> shellActive shell)
                    (\txt -> case txt of
                                "enable" -> Right <$> startShell shell
                                "disable" -> Right <$> stopShell shell
                                _ -> pure $ Left eINVAL
                    )
                 )
    viewsFile <- ("views", ) <$> makeViewFile shell

    pure $ DirEntry $ simpleDir $ M.fromList $ [active, viewsFile]

enumerateShells :: (FocusCore vs a, WSTag a) => Way vs a (Map String (Entry vs a))
enumerateShells = do
    shells <- wayCoreShells <$> getState

    fmap M.fromList . forM shells $ \shell -> do
        entry <- makeShellDir shell
        name <- shellName shell
        pure (T.unpack $ name, entry)

shellsDir :: (FocusCore vs a, WSTag a) => Entry vs a
shellsDir = DirEntry $ enumeratingDir enumerateShells
