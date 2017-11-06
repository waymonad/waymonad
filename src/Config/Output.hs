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

        pure $ OutputConfig
            { outName = name
            , outPosition = pos
            , outMode = mode
            }

