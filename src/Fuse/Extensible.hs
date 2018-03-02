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
