{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Config.Input
where

import Config.Schema
import Data.Traversable (for)
import Data.Maybe (catMaybes)

import Data.Text (Text)

import qualified Data.Text as T
import qualified Waymonad.Input.Libinput as LI

newtype LIOptions = LIOptions [(LI.LibinputOption, Text)] deriving (Show)

data InputConfig = InputConfig
    { inputCName    :: Text
    , inputCOptions :: LIOptions
    , inputCSeat    :: Maybe Text
    } deriving (Show)

instance Spec LIOptions where
    valuesSpec = sectionsSpec "input options" $
        let makeDesc name = "The " `T.append` name `T.append` " option."
            makeOpt opt = optSection (LI.optionName opt) (makeDesc $ LI.optionName opt)
            opts = map (\o -> (makeOpt o, o)) LI.libinputOptions
            ret = for opts $ \(sec, opt) ->
                fmap (opt, ) <$> sec
         in (LIOptions . catMaybes) <$> ret

instance Spec InputConfig where
    valuesSpec = sectionsSpec "input" $ do
        name  <- reqSection "name"  "Name of the device"
        seat <- optSection "seat" "Seat this device should be assigned to"
        options <- reqSection "options" "The options that should be set"

        pure $ InputConfig name options seat

