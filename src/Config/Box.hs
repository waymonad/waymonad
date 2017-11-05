{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
module Config.Box
where

import Config.Schema

data Point a = Point
    { pointX :: a
    , pointY :: a
    } deriving (Eq, Show)

instance Spec a => Spec (Point a) where
    valuesSpec = sectionsSpec "point" $ do
        x <- reqSection "x" "The x position of the point"
        y <- reqSection "y" "The y position of the point"

        pure $ Point x y
