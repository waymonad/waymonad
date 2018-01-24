{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2018  Markus Ongyerth

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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Layout.AvoidStruts
where

import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Output (Output(outputName))
import Utility (whenJust)
import ViewSet
import WayUtil (sendMessageOn)
import Waymonad.Types

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import qualified Data.Text as T


avoidStruts :: l -> StrutAvoider l
avoidStruts = StrutAvoider (Struts 0 0 0 0)

data Struts = Struts
    { strutsNorth :: Int
    , strutsSouth :: Int
    , strutsEast  :: Int
    , strutsWest  :: Int
    } deriving (Show, Eq)

data StrutAvoider l = StrutAvoider
    { avoiderStruts :: Struts
    , avoiderChild  :: l
    }

newtype StrutMessage = StrutMessage Struts deriving (Show)

instance Message StrutMessage

instance LayoutClass l => LayoutClass (StrutAvoider l) where
    handleMessage s@(StrutAvoider st child) m = case getMessage m of
        Just (StrutMessage x) -> Just s { avoiderStruts = x }
        Nothing -> StrutAvoider st <$> handleMessage child m
    broadcastMessage s@(StrutAvoider st child) m = case getMessage m of
        Just (StrutMessage x) -> Just s { avoiderStruts = x }
        Nothing -> StrutAvoider st <$> broadcastMessage child m
    description StrutAvoider {avoiderChild = child} =
        "StrutAvoider(" `T.append` description child `T.append` ")"
    currentDesc StrutAvoider {avoiderChild = child} =
        "StrutAvoider(" `T.append` currentDesc child `T.append` ")"

instance (FocusCore vs ws, GenericLayoutClass l vs ws) => GenericLayoutClass (StrutAvoider l) vs ws where
    pureLayout (StrutAvoider (Struts n s e we) l) vs ws (WlrBox x y wi h) =
        pureLayout l vs ws $ WlrBox
            (x + we)
            (y + n)
            (wi - (e + we))
            (h - (n + s))

setWSStruts :: (FocusCore vs ws, WSTag ws, Layouted vs ws)
            => ws -> Struts -> Way vs ws ()
setWSStruts ws struts = sendMessageOn ws (StrutMessage struts)

constStrutHandler :: (FocusCore vs ws, WSTag ws, Layouted vs ws)
                  => [(Text, Struts)] -> OutputMappingEvent ws -> Way vs ws ()
constStrutHandler xs (OutputMappingEvent out _ now) =
    let struts = lookup (outputName out) xs
     in whenJust now $ \ws -> setWSStruts ws $ fromMaybe (Struts 0 0 0 0) struts
