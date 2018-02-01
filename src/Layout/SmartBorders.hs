{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Layout.SmartBorders
where

import Formatting

import ViewSet
import Waymonad.Types
import WayUtil.SSD

import qualified Data.Text as T

data SmartBorders l = SmartBorders Int l

mkSmartBorders :: Int -> l -> SmartBorders l
mkSmartBorders = SmartBorders

instance LayoutClass l => LayoutClass (SmartBorders l) where
    handleMessage (SmartBorders state l) m = SmartBorders state <$> handleMessage l m
--        case getMessage m of
--        (Just ToggleFullM) -> Just $ ToggleFull (fmap not state) l
--        (Just SetFull) -> Just $ ToggleFull (Identity True) l
--        (Just UnsetFull) -> Just $ ToggleFull (Identity False) l
--        Nothing -> ToggleFull state <$> handleMessage l m
    broadcastMessage (SmartBorders state l) m = SmartBorders state <$> broadcastMessage l m
--        (Just ToggleFullM) -> Just $ ToggleFull (fmap not state) l
--        (Just SetFull) -> Just $ ToggleFull (Identity True) l
--        (Just UnsetFull) -> Just $ ToggleFull (Identity False) l
--        Nothing -> ToggleFull state <$> broadcastMessage l m
    description (SmartBorders _ l) = "SmartBorders(" `T.append` description l `T.append` ")"
    currentDesc (SmartBorders v l) = sformat ("SmartBorders[" % int % "]" % stext) v (currentDesc l)

instance (GenericLayoutClass l vs ws) => GenericLayoutClass (SmartBorders l) vs ws where
    pureLayout (SmartBorders w l) vs ws box = case pureLayout l vs ws box of

        [] -> []
        [x] -> [x]
        xs -> flip fmap xs (\case
            (v, NoSSD s, b) -> (v, sillyDeco w s, b)
            x -> x)
