{-# LANGUAGE OverloadedStrings #-}
module Layout.Tall
where

import Data.Foldable (toList)

import Graphics.Wayland.WlRoots.Box (WlrBox(..))

import ViewSet

data Tall = Tall

instance LayoutClass Tall where
    description _ = "Tall"
    handleMessage _ _ = Nothing
    pureLayout _ box zipper = case toList zipper of
        [x] -> [(x, box)]
        (x:xs)->
            let width = boxWidth box `div` 2
                master = (x, box { boxWidth = width  })
                slaves = zip xs [0 ..]
                num = length xs
                height = boxHeight box `div` num
                ibox i = box
                    { boxWidth = width
                    , boxX = boxX box + width
                    , boxHeight = height
                    , boxY = boxY box + i * height
                    }
             in master : map (\(v, i) -> (v, ibox i)) slaves
        [] -> []
