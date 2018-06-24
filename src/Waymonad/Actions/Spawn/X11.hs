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
module Waymonad.Actions.Spawn.X11
    ( manageX11SpawnOn
    , spawnX11On
    )
where

import Control.DeepSeq (NFData (..), force)
import Control.Monad.IO.Class (liftIO)
import Data.List (lookup)
import Data.Typeable (Typeable)
import System.Posix.Types (ProcessID)
import System.Posix.Process (forkProcess, executeFile)

import Waymonad.Shells.XWayland (xwayGetPid)
import Waymonad.View (getViewInner)
import Waymonad.ViewSet (WSTag)
import Waymonad.Extensible (ExtensionClass (..))
import Waymonad.Managehook (Managehook, query, liftWay, InsertAction (InsertInto))
import Waymonad.Utility.Extensible (getEState , modifyEState)
import Waymonad.Utility.Base (doJust, whenJust)
import Waymonad.Types (Way)

newtype PidT = PidT ProcessID deriving (Eq)
instance NFData PidT where rnf = flip seq ()

newtype X11Spawner ws = X11Spawner { spawnPids :: [(PidT, ws)] } deriving Typeable

instance Typeable ws => ExtensionClass (X11Spawner ws) where
    initialValue = X11Spawner []

modifySpawner :: (NFData ws, Typeable ws) => ([(PidT, ws)] -> [(PidT, ws)]) -> Way vs ws ()
modifySpawner fun = modifyEState (X11Spawner . force . fun . spawnPids)

manageX11SpawnOn :: (NFData ws, WSTag ws) => Managehook vs ws
manageX11SpawnOn = do
    view <- query
    liftWay $ doJust (getViewInner view) $ \xway -> 
        doJust (fmap PidT <$> xwayGetPid xway) $ \pid -> do
            X11Spawner pids <- getEState
            whenJust (lookup pid pids) $ \ws -> do
                modifySpawner (filter ((/=) pid . fst))
                pure $ InsertInto ws

execChild :: String -> [String] -> IO a
execChild name args = executeFile name True args Nothing

spawnX11On :: (NFData ws, Typeable ws) => ws -> String -> [String] -> Way vs ws ()
spawnX11On ws name args = do
    pid <- liftIO $ forkProcess $ execChild name args
    modifySpawner ((:) (PidT pid, ws) . take 19)
