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
{-# LANGUAGE ScopedTypeVariables #-}
module Fuse.Extensible
    ( extensibleDir
    )
where

import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import Data.Text (Text)

import Waymonad.IPC
import Waymonad.Types (Way)
-- import Waymonad.Utility.Base

import Fuse.Common

import qualified Data.Map as M
import qualified Data.Text as T

makeEntry :: (Typeable vs, Typeable ws) => IPCEntry vs ws -> Way vs ws (Maybe (Entry vs ws))
makeEntry e@(IPCEntry _ _ rCheck wCheck) = do
    isR <- rCheck
    isW <- wCheck
    pure $ case isR || isW of
        False -> Nothing
        True -> Just . FileEntry $ ipcFile (getEntryReadFun e) (if isW then getEntryWriteFun e else Nothing)


makeGroup :: forall vs ws. (Typeable vs, Typeable ws) => IPCGroup vs ws -> Entry vs ws
makeGroup (IPCGroup xs) = DirEntry $ enumeratingDir (M.fromList . catMaybes <$> mapM handleElem xs)
    where   handleElem :: (Text, Either (IPCGroup vs ws) (IPCEntry vs ws)) -> Way vs ws (Maybe (String, Entry vs ws))
            handleElem (name, Left grp) = pure $ Just (T.unpack name, makeGroup grp)
            handleElem (name, Right entry) = fmap (T.unpack name,) <$> makeEntry entry

extensibleDir :: (Typeable vs, Typeable ws) => IPCGroup vs ws -> Entry vs ws
extensibleDir = makeGroup
