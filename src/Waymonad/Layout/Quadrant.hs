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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Waymonad.Layout.Quadrant
    ( QuadrantSet
    , setupQuadrant
    , sendToQ
    , Quadrant (..)
    )
where

import Control.Applicative ((<|>))
import Data.Default (Default(..))
import Data.List (nub)
import Data.Map (Map)
import Data.Semigroup ((<>))

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Input.Seat (Seat)
import ViewSet

import qualified Data.Map as M
import qualified Data.Set as S

data Quadrant = TL | TR | BL | BR deriving (Eq, Show)

setupQuadrant :: ([ws] -> child) -> [ws] -> QuadrantSet child ws
setupQuadrant fun xs = QuadrantSet (fun xs) (fun xs) (fun xs) (fun xs) mempty


data QuadrantSet child ws = QuadrantSet
    { quadrantTL :: child
    , quadrantTR :: child
    , quadrantBL :: child
    , quadrantBR :: child
    , quadrantFocus :: (Map Seat Quadrant)
    }

instance Default child => Default (QuadrantSet child ws) where
    def = QuadrantSet def def def def mempty

withQuadrant :: Quadrant -> QuadrantSet child ws -> (child -> a) -> a
withQuadrant TL q f = f $ quadrantTL q
withQuadrant TR q f = f $ quadrantTR q
withQuadrant BL q f = f $ quadrantBL q
withQuadrant BR q f = f $ quadrantBR q

updateQuadrant :: Quadrant -> QuadrantSet child ws -> (child -> child) -> QuadrantSet child ws
updateQuadrant TL q f = q { quadrantTL = f $ quadrantTL q }
updateQuadrant TR q f = q { quadrantTR = f $ quadrantTR q }
updateQuadrant BL q f = q { quadrantBL = f $ quadrantBL q }
updateQuadrant BR q f = q { quadrantBR = f $ quadrantBR q }

updateAll :: QuadrantSet child ws -> (child -> child) -> QuadrantSet child ws
updateAll set fun =
    updateQuadrant TL (updateQuadrant TR (updateQuadrant BL (updateQuadrant BR set fun) fun) fun) fun

sendToQ :: (FocusCore child ws, Eq ws) => Quadrant -> Seat -> ws -> QuadrantSet child ws -> QuadrantSet child ws
sendToQ q seat ws set =
    case _getFocused set ws (Just seat) of
        Nothing -> set
        Just view -> updateQuadrant q (_removeView ws view set) (_insertView ws (Just seat) view)

instance (Eq ws, FocusCore child ws) => FocusCore (QuadrantSet child ws) ws where
        -- TODO: Filter focus to focused quadrant
        _getViews (QuadrantSet tl tr bl br _) ws =
            let tlvs = _getViews tl ws
                trvs = _getViews tr ws
                blvs = _getViews bl ws
                brvs = _getViews br ws
             in tlvs <> trvs <> blvs <> brvs
        _insertView ws seat view set = case seat of
            Just s -> case M.lookup s (quadrantFocus set) of
                Just q -> updateQuadrant q set (_insertView ws seat view)
                Nothing -> let set' = set { quadrantFocus = M.insert s TL $ quadrantFocus set }
                            in updateQuadrant TL set' (_insertView ws seat view)
            Nothing -> updateQuadrant TL set (_insertView ws seat view)
        _removeView ws view set = updateAll set (_removeView ws view)
        removeGlobal v ws q = updateAll q (removeGlobal v ws)
        _getFocused set@(QuadrantSet tl tr bl br m) ws seat = case seat of
            Just s -> case M.lookup s m of
                Just q -> withQuadrant q set (\vs -> _getFocused vs ws seat)
                Nothing -> Nothing
            Nothing ->
                let tlvs = _getFocused tl ws seat
                    trvs = _getFocused tr ws seat
                    blvs = _getFocused bl ws seat
                    brvs = _getFocused br ws seat
                in tlvs <|> trvs <|> blvs <|> brvs
        _focusView ws seat view set@(QuadrantSet tl tr bl br m) =
            let tlM = if view `elem` (fmap snd . S.toList $ _getViews tl ws) then Just TL else Nothing
                trM = if view `elem` (fmap snd . S.toList $ _getViews tr ws) then Just TR else Nothing
                blM = if view `elem` (fmap snd . S.toList $ _getViews bl ws) then Just BL else Nothing
                brM = if view `elem` (fmap snd . S.toList $ _getViews br ws) then Just BR else Nothing
                qM = tlM <|> trM <|> blM <|> brM
             in case qM of
                    Nothing -> set
                    Just q -> let set' = set { quadrantFocus = M.insert seat q m }
                               in updateQuadrant q set' (_focusView ws seat view)
        getLayouted (QuadrantSet tl tr bl br _) ws box =
            let width = boxWidth box `div` 2
                height = boxHeight box `div` 2
                box' = box {boxWidth = width, boxHeight = height }
                tlvs = getLayouted tl ws box'
                trvs = getLayouted tr ws box' { boxX = boxX box + width }
                blvs = getLayouted bl ws box' { boxY = boxY box + height }
                brvs = getLayouted br ws box' { boxX = boxX box + width, boxY = boxY box + height }
             in tlvs <> trvs <> blvs <> brvs

instance Layouted vs ws => Layouted (QuadrantSet vs ws) ws where
    messageWS m ws vs = updateAll vs (messageWS m ws)
    broadcastWS m ws vs = updateAll vs (broadcastWS m ws)
    broadcastVS m ws vs = updateAll vs (broadcastVS m ws)
    getLayout _ _ = Nothing
