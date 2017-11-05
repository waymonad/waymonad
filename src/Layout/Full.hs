{-# LANGUAGE OverloadedStrings #-}
module Layout.Full
where

import ViewSet

data Full = Full

instance LayoutClass Full where
    description _ = "Full"
    handleMessage _ _ = Nothing
    pureLayout _ box zipper = case getMaster' zipper of
        Nothing -> []
        Just v -> [(v, box)]
