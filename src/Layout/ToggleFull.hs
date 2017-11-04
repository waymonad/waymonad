{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
module Layout.ToggleFull
where

import Control.Applicative ((<|>))
import Data.Text (Text)

import Graphics.Wayland.WlRoots.Box (WlrBox)

import ViewSet
    ( Message
    , Zipper
    , LayoutClass (..)
    , SomeMessage
    , getFirstFocused'
    , getMaster'
    , getMessage
    )

import qualified Data.Text as T

data TMessage = TMessage
    deriving (Show, Eq, Message)

data ToggleFull l = ToggleFull Bool l

instance LayoutClass l => LayoutClass (ToggleFull l) where
    handleMessage :: ToggleFull l -> SomeMessage -> Maybe (ToggleFull l)
    handleMessage (ToggleFull state l) m =
        case getMessage m of
            (Just TMessage) -> Just $ ToggleFull (not state) l
            Nothing -> Nothing
    description :: ToggleFull l -> Text
    description (ToggleFull _ l) =
        "ToggleFull(" `T.append` description l `T.append` ")"
    pureLayout :: ToggleFull l -> WlrBox -> Zipper b c -> [(c, WlrBox)]
    pureLayout (ToggleFull False l) box z = pureLayout l box z
    pureLayout (ToggleFull True _) box z =
        case getFirstFocused' z <|> getMaster' z of
            Nothing -> []
            Just v -> [(v, box)]
