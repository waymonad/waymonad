{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Waymonad.Layout.Vertical
where

import Data.Set (Set)

import Graphics.Wayland.WlRoots.Box (WlrBox(..))

import Waymonad.ViewSet
import Waymonad.Types (SSDPrio (NoSSD))
import Waymonad.Types.Core (Seat, View)

data Vertical = Vertical

instance LayoutClass Vertical where
    description _ = "Vertical"
    handleMessage _ _ _ = Nothing
    broadcastMessage _ _  = Nothing

instance ListLike vs ws => GenericLayoutClass Vertical vs ws where
    pureLayout _ vs ws box = layoutVertical box (_asList vs ws)

layoutVertical :: WlrBox -> [(Set Seat, View)] -> [(View, SSDPrio, WlrBox)]
layoutVertical box xs =
    let slaves = zip xs [0 ..]
        num = length xs
        height = boxHeight box `div` num
        ibox i = box
            { boxHeight = height
            , boxY = boxY box + i * height
            }
    in map (\((s, v), i) -> (v, NoSSD s, ibox i)) slaves
