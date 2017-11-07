{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
module Layout.Mirror
    ( MMessage (..)
    , Mirror (..)
    )
where

import Data.Text (Text)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import ViewSet
    ( Message
    , Zipper
    , LayoutClass (..)
    , SomeMessage
    , getMessage
    )

import qualified Data.Text as T

data MMessage = MMessage
    deriving (Show, Eq, Message)

data Mirror l = Mirror Bool l

instance LayoutClass l => LayoutClass (Mirror l) where
    handleMessage :: Mirror l -> SomeMessage -> Maybe (Mirror l)
    handleMessage (Mirror state l) m =
        case getMessage m of
            (Just MMessage) -> Just $ Mirror (not state) l
            Nothing -> Mirror state <$> handleMessage l m
    description :: Mirror l -> Text
    description (Mirror _ l) =
        "Mirror(" `T.append` description l `T.append` ")"
    pureLayout :: Mirror l -> WlrBox -> Zipper b c -> [(c, WlrBox)]
    pureLayout (Mirror False l) box z = pureLayout l box z
    pureLayout (Mirror True l) box z =
        fmap (fmap mirrorBox) $ pureLayout l (mirrorBox box) z

mirrorBox :: WlrBox -> WlrBox
mirrorBox (WlrBox x y w h) = WlrBox y x h w
