{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Layout.SmartBorders
where

import Data.Functor.Identity
import Formatting

import ViewSet
import Waymonad.Types
import WayUtil.SSD

import qualified Data.Text as T

data SetBorderWidth = SetBorderWidth Int deriving (Eq, Show)

instance Message SetBorderWidth

data SmartBorders c l = SmartBorders
    { smartBorderWidth :: c Int
    , smartBorderChild :: l
    }

mkSmartBorders :: Int -> l -> SmartBorders Identity l
mkSmartBorders = SmartBorders . Identity

instance LayoutClass l => LayoutClass (SmartBorders Identity l) where
    handleMessage (SmartBorders state l) m = case getMessage m of
        (Just (SetBorderWidth w)) -> Just $ SmartBorders (Identity w) l
        Nothing -> SmartBorders state <$> handleMessage l m
    broadcastMessage (SmartBorders state l) m = case getMessage m of
        (Just (SetBorderWidth w)) -> Just $ SmartBorders (Identity w) l
        Nothing -> SmartBorders state <$> broadcastMessage l m
    description (SmartBorders _ l) = "SmartBorders(" `T.append` description l `T.append` ")"
    currentDesc (SmartBorders (Identity v) l) = sformat ("SmartBorders[" % int % "]" % stext) v (currentDesc l)

instance (GenericLayoutClass l vs ws) => GenericLayoutClass (SmartBorders Identity l) vs ws where
    pureLayout (SmartBorders (Identity w) l) vs ws box = case pureLayout l vs ws box of

        [] -> []
        [x] -> [x]
        xs -> flip fmap xs (\case
            (v, NoSSD s, b) -> (v, sillyDeco w s, b)
            x -> x)
