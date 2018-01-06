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
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
module Config.Output (OutputConfig (..), Mode (..))
where

import Config.Schema
import Data.Text (Text)

import Config.Box (Point (..))

data OutputConfig = OutputConfig
    { outName :: Text
    -- This should be a point? or even Box?
    , outPosition :: Maybe (Point Int)
    , outMode :: Maybe Mode
    , outScale :: Maybe Float
    } deriving (Eq, Show)

data Mode = Mode
    { modeCWidth :: Word
    , modeCHeight :: Word
    , modeCRefresh :: Word
    } deriving (Eq, Show)


instance Spec Mode where
    valuesSpec = sectionsSpec "mode" $ do
        width  <- reqSection "width"  "The width of the output"
        height <- reqSection "height" "The height of the output"
        refresh <- reqSection "refresh-rate" "The refresh rate"

        pure $ Mode width height refresh


instance Spec OutputConfig where
    valuesSpec = sectionsSpec "output" $ do
        name <- reqSection "name" "Output name (actually connector)"
        pos <- optSection "position" "The position of the output"
        mode <- optSection "mode" "The mode that should be set for this output"
        scale <- optSection "scale" "The output scale"

        pure OutputConfig
            { outName = name
            , outPosition = pos
            , outMode = mode
            , outScale = fmap fromRational scale
            }

