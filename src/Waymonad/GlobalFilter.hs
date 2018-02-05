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
{-|
Module      : GlobalFilter
Description : Provides a way to filter the wayland protocols available to clients
Maintainer  : ongy
Stability   : testing
Portability : Linux

The brackets for extensions should register themselves with this module.
This can then be used to filter the access clients get.

These filters can be interesting for containerized clients/priviliged protocols.
E.g. the screenshooter should not be available to every client, like the browser.
-}
module Waymonad.GlobalFilter
    ( registerGlobal
    , unregisterGlobal
    , getGlobalName
    , getFilterBracket
    , filterKnown
    , filterUser
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import System.Posix.User (getEffectiveUserID)

import Graphics.Wayland.Server (DisplayServer, Client (..), clientGetCredentials)
import Graphics.Wayland.Global (WlGlobal, setGlobalFilter)

import Waymonad.Start (Bracketed (..))
import Waymonad (makeCallback2)
import Waymonad.Types (Way)
import Waymonad.Extensible (ExtensionClass (..))
import Waymonad.Utility.Extensible (modifyEState, getEState)

import qualified Data.Map as M

newtype GlobalMap  = GlobalMap { unGM :: Map (Ptr WlGlobal) Text }

instance ExtensionClass GlobalMap where
    initialValue = GlobalMap mempty

-- | Register a wl_global with the filter. Should be used by module builders.
registerGlobal :: Text -> Ptr WlGlobal -> Way vs a ()
registerGlobal name ptr =
    modifyEState (GlobalMap . M.insert ptr name . unGM)

-- | Remove the registry entry when the global is removed for some reason.
unregisterGlobal :: Ptr WlGlobal -> Way vs a ()
unregisterGlobal ptr =
    modifyEState (GlobalMap . M.delete ptr . unGM)

-- | Get the name of the global to be filtered. This should be used by filter
-- functions
getGlobalName :: Ptr WlGlobal -> Way vs a (Maybe Text)
getGlobalName name = M.lookup name . unGM <$> getEState

-- No unregister for now, it's not worth it either way :/
{- | Set the filter to the provided function. This will get called for every
global before it is exposed to clients.
It should return True if the global should be emitted and False if it should be hidden.

> getFilterBracket filterUser
-}
getFilterBracket :: (Client -> Ptr WlGlobal -> Way vs a Bool) -> Bracketed vs DisplayServer a
getFilterBracket fun = Bracketed (\dsp -> do
    cb <- makeCallback2 (fun . Client)
    liftIO (setGlobalFilter dsp $ Just cb)
                                 ) (const $ pure ())

-- | This is mostly to show off the filter. But could be used to build other
-- filters.
filterKnown :: Client -> Ptr WlGlobal -> Way vs a Bool
filterKnown _ ptr = isNothing <$> getGlobalName ptr

-- | Only give clients that have the same uid as the compositor access to
-- globals that registered with us.
filterUser :: Client  -> Ptr WlGlobal -> Way vs a Bool
filterUser client ptr = do
    mine <- liftIO $ getEffectiveUserID --- This could be cached?
    (_, other, _) <- liftIO $ clientGetCredentials client
    if mine == other
        then pure True
        else filterKnown client ptr
