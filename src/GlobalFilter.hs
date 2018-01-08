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
module GlobalFilter
    ( registerGlobal
    , unregisterGlobal
    , getGlobalName
    , getFilterBracket
    , filterKnown
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Server (DisplayServer, Client)
import Graphics.Wayland.Global (WlGlobal, setGlobalFilter)

import Shared (Bracketed (..))
import Waymonad (makeCallback2)
import Waymonad.Types (Way)
import Waymonad.Extensible (ExtensionClass (..))
import WayUtil (modifyEState, getEState)

import qualified Data.Map as M

newtype GlobalMap  = GlobalMap { unGM :: Map (Ptr WlGlobal) Text }

instance ExtensionClass GlobalMap where
    initialValue = GlobalMap mempty


registerGlobal :: Text -> Ptr WlGlobal -> Way a ()
registerGlobal name ptr =
    modifyEState (GlobalMap . M.insert ptr name . unGM)

unregisterGlobal :: Ptr WlGlobal -> Way a ()
unregisterGlobal ptr =
    modifyEState (GlobalMap . M.delete ptr . unGM)

getGlobalName :: Ptr WlGlobal -> Way a (Maybe Text)
getGlobalName name = M.lookup name . unGM <$> getEState

-- | No unregister for now, it's not worth it either way :/
getFilterBracket :: (Ptr Client -> Ptr WlGlobal -> Way a Bool) -> Bracketed DisplayServer a
getFilterBracket fun = Bracketed (\dsp -> do
    cb <- makeCallback2 fun
    liftIO (setGlobalFilter dsp $ Just cb)
                                 ) (const $ pure ())

-- This is pretty useless, just to show off
filterKnown :: Ptr Client -> Ptr WlGlobal -> Way a Bool
filterKnown _ ptr = isNothing <$> getGlobalName ptr
