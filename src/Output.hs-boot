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
module Output
    ( Output (..)
    , getOutputId
    , setOutputDirty
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Text (Text)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Output (WlrOutput)

import View (View) 

data Output = Output
    { outputRoots  :: Ptr WlrOutput
    , outputName   :: Text
    , outputActive :: IORef Bool
    , outputLayout :: [IORef [(View, WlrBox)]]
    , outputLayers :: Map Text (IORef [(View, WlrBox)])
    }

instance Show Output
instance Eq Output

getOutputId :: Output -> Int

setOutputDirty :: MonadIO m => Output -> m ()
