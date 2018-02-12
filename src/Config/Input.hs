{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE TupleSections #-}
module Config.Input
where

import Config.Schema
import Control.Monad (forM)
import Data.Functor.Alt ((<!>))
import Data.Maybe (catMaybes)

import Data.Text (Text)

import qualified Data.Text as T
import qualified Waymonad.Input.Libinput as LI

newtype LIOptions = LIOptions [(LI.LibinputOption, Text)]

data InputConfig = InputConfig
    { inputCName   :: Text
    , inputOptions :: LIOptions
    , inputSeat    :: Maybe Text
    }

instance Spec LIOptions where
    valuesSpec = sectionsSpec "input options" $ do
        let makeDesc name = "The " `T.append` name `T.append` " option."
            makeOpt opt = optSection (LI.optionName opt) (makeDesc $ LI.optionName opt)
            opts = map (\o -> (makeOpt o, o)) LI.libinputOptions
        options <- forM opts $ \(opt, val) -> do
            ret <- opt
            pure $ (val,) <$> ret
        pure . LIOptions $ catMaybes options

instance Spec InputConfig where
    valuesSpec = sectionsSpec "input" $ do
        name  <- reqSection "name"  "Name of the device"
        seat <- optSection "seat" "Seat this device should be assigned to"
        options <- reqSection "options" "The options that should be set"

        pure $ InputConfig name options seat

