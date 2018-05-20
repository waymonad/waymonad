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
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Config.Output
    ( OutputConfig (..)
    , Mode (..)
    , modifyOutputConfig
    )
where

import Config.Schema
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Alt ((<!>))
import Data.List (sortOn)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Ratio (Ratio, (%))
import Data.Text (Text)
import Foreign.Ptr (Ptr)
import Foreign.Storable

import Graphics.Wayland.Server
    ( OutputTransform
    , outputTransformNormal
    , outputTransform180
    , outputTransform90
    , outputTransform270
    , outputTransformFlipped
    , outputTransformFlipped_180
    , outputTransformFlipped_90
    , outputTransformFlipped_270
    )
import Graphics.Wayland.WlRoots.Output

import Waymonad.Output (setPreferdMode, addOutputToWork, setOutMode)
import Waymonad.Utility.Base (whenJust)
import Waymonad.Types

import Config.Box (Point (..), asRootsPoint)
import Waymonad.Main (WayUserConf (..))

import qualified Data.Map as M

data OutputConfig = OutputConfig
    { outName :: Text
    -- This should be a point? or even Box?
    , outPosition :: Maybe (Point Int)
    , outMode :: Maybe Mode
    , outScale :: Maybe Float
    , outTransform :: Maybe OutputTransform
    } deriving (Eq, Show)

data Mode = Mode
    { modeCWidth :: Word
    , modeCHeight :: Word
    , modeCRefresh :: Word
    } deriving (Eq, Show)

instance Spec OutputTransform where
    valuesSpec = 
        outputTransformNormal      <$ atomSpec "Normal"     <!>
        outputTransform90          <$ atomSpec "N90"         <!>
        outputTransform180         <$ atomSpec "N180"        <!>
        outputTransform270         <$ atomSpec "N270"        <!>
        outputTransformFlipped     <$ atomSpec "Flipped"    <!>
        outputTransformFlipped_90  <$ atomSpec "Flipped90"  <!>
        outputTransformFlipped_180 <$ atomSpec "Flipped180" <!>
        outputTransformFlipped_270 <$ atomSpec "Flipped270"

instance Spec Mode where
    valuesSpec = sectionsSpec "mode" $ do
        width  <- reqSection "width"  "The width of the output in pixels"
        height <- reqSection "height" "The height of the output in pixels"
        refresh <- reqSection "refresh-rate" "The refresh rate"

        pure $ Mode width height refresh


instance Spec OutputConfig where
    valuesSpec = sectionsSpec "output" $ do
        name <- reqSection "name" "Output name (actually connector)"
        pos <- optSection "position" "The logical position of the output"
        mode <- optSection "mode" "The output mode. This is pre transformation/scaling"
        scale <- optSection "scale" "The output scale"
        transform <- optSection "transform" "The output transformation"

        pure OutputConfig
            { outName = name
            , outPosition = pos
            , outMode = mode
            , outScale = fmap fromRational scale
            , outTransform = transform
            }


-- Should this be in another file..?

pickMode
    :: Ptr WlrOutput
    -> Maybe Mode
    -> Way vs ws (Maybe (Ptr OutputMode))
-- If there's no config, just pick the "last" mode, it's the native resolution
pickMode output Nothing = liftIO $ do
    modes <- getModes output
    pure $ listToMaybe $ reverse modes
pickMode output (Just cfg) = liftIO $ do
    modes <- getModes output
    paired <- forM modes $ \x -> do
        marshalled <- peek x
        pure (marshalled, x)
    -- First try to find modes that match *exactly* on resolution
    let matches = map snd . sortOn (refreshDist . fst) $ filter (sameResolution . fst) paired
    let ratio = map snd . sortOn (\m -> (resDist $ fst m, refreshDist $ fst m)) $ filter (sameAspect . fst) paired

    -- TODO: Sanitize this
    pure . listToMaybe . reverse $ modes ++ ratio ++ matches
    where   sameResolution :: OutputMode -> Bool
            sameResolution mode =
                fromIntegral (modeWidth mode) == modeCWidth cfg
                && fromIntegral (modeHeight mode) == modeCHeight cfg
            refreshDist :: OutputMode -> Int -- Cast to Int, so we don't get wrapping arithmetic, *should* be big enough!
            refreshDist mode = abs $ fromIntegral (modeRefresh mode) - fromIntegral (modeCRefresh cfg)
            confAspect :: Ratio Word
            confAspect = modeCWidth cfg % modeCHeight cfg
            aspect :: OutputMode -> Ratio Word
            aspect mode = fromIntegral (modeWidth mode) % fromIntegral (modeHeight mode)
            sameAspect :: OutputMode -> Bool
            sameAspect = (==) confAspect . aspect
            resDist :: OutputMode -> Int -- We know it's the same ration, so be lazy here
            resDist mode = abs $ fromIntegral (modeWidth mode) - fromIntegral (modeCWidth cfg)


configureOutput
    :: OutputConfig
    -> Output
    -> Way vs a ()
configureOutput conf out@Output {outputRoots = output} = do
    let position = outPosition conf
        confMode = outMode conf
        transform = outTransform conf
    mode <- pickMode output confMode

    liftIO $ whenJust transform (transformOutput output)
    liftIO $ whenJust (outScale conf) (setOutputScale output)

    let setMode = case mode of
            Just m -> setOutMode output m
            Nothing -> setPreferdMode output

    setMode $  addOutputToWork out (fmap asRootsPoint position)


prependConfig :: Map Text OutputConfig -> (Output -> Way vs ws ()) -> (Output -> Way vs ws ())
prependConfig configs others output =
    case M.lookup (outputName output) configs of
        Just config -> configureOutput config output
        Nothing -> others output


modifyOutputConfig :: Map Text OutputConfig -> WayUserConf vs ws -> WayUserConf vs ws
modifyOutputConfig m conf = conf { wayUserConfOutputAdd = prependConfig m $ wayUserConfOutputAdd conf }
