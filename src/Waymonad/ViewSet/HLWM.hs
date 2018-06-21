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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Waymonad.ViewSet.HLWM
where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Semigroup ((<>))
import Data.Map (Map)
import Data.Maybe (fromMaybe, fromJust, listToMaybe, maybeToList)
import Data.Set (Set)
import Data.Text (Text)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

import Waymonad.ViewSet
import Waymonad.Types (SSDPrio)
import Waymonad.Types.Core (View, Seat)

import qualified Waymonad.ViewSet.Utility as U

import qualified Data.Set as S
import qualified Data.Map.Strict as M

type HLWorkspace ws = U.Workspace (U.Zipper Seat View) ws

data Orientation = Horizontal | Vertical deriving (Eq, Show)
data Direction = HLUp | HLDown | HLLeft | HLRight deriving (Eq, Show)
data HLFocus = FocusFirst | FocusSecond deriving (Eq, Show)

data Workspace ws
    = SplitWS Orientation Double (Map Seat HLFocus) (Workspace ws) (Workspace ws)
    | LeafWS { unLeafWS :: HLWorkspace ws }
    deriving (Show)

data HLWMMessage
    = HLWMSplit Orientation Double
    | SwitchOrientation
    | HLWMMerge
    | SendOver
    | ChangeSize Direction (Double -> Double)

instance Message HLWMMessage

data ViewSet ws = ViewSet
    { _viweSetWorkspaces :: Map ws (Workspace ws)
    , _viewSetLayout :: GenericLayout (U.Zipper Seat View) ws
    }

splitWS :: HLWMMessage -> Workspace ws -> Workspace ws
splitWS (HLWMSplit o d) (LeafWS (U.Workspace l (Just (U.Zipper (x@(f, _):xs@(_:_)))))) =
    let empty = LeafWS $ U.Workspace l (Just $ U.Zipper [x])
     in SplitWS o d (M.fromList . fmap (, FocusSecond) $ S.toList f)  (LeafWS $ U.Workspace l (Just $ U.Zipper xs)) empty
splitWS _ ws = ws

switchOrientation :: Workspace ws -> Workspace ws
switchOrientation ws@LeafWS {} = ws
switchOrientation (SplitWS Horizontal d m f s) = SplitWS Vertical d m f s
switchOrientation (SplitWS Vertical d m f s) = SplitWS Horizontal d m f s

addChildren :: [(Set Seat, View)] -> Workspace ws -> Workspace ws
addChildren vs (LeafWS (U.Workspace l Nothing)) = LeafWS (U.Workspace l $ Just $ U.Zipper vs)
addChildren vs (LeafWS (U.Workspace l (Just (U.Zipper xs)))) =
    let seats = foldMap fst vs
        tmap fun (a, b) = (fun a, b)
        ys = fmap (tmap (S.\\ seats)) xs
     in (LeafWS (U.Workspace l (Just (U.Zipper (vs ++ ys)))))
addChildren vs (SplitWS o r m first second) = SplitWS o r m (addChildren vs first) second

mergeLeafes :: Workspace ws -> Workspace ws
mergeLeafes (SplitWS _ _ _ first (LeafWS (U.Workspace _ z))) = 
    addChildren (maybe [] U.unZipper z) first
mergeLeafes (SplitWS _ _ _ (LeafWS (U.Workspace _ z)) second) = 
    addChildren (maybe [] U.unZipper z) second
mergeLeafes ws = ws

mapWorkspace :: (HLWorkspace ws -> HLWorkspace ws) -> Workspace ws -> Workspace ws
mapWorkspace fun (LeafWS ws) = LeafWS (fun ws)
mapWorkspace fun (SplitWS o d m l r) = SplitWS o d m (mapWorkspace fun l) (mapWorkspace fun r)

mapWSMaybe :: (HLWorkspace ws -> Maybe (HLWorkspace ws)) -> Workspace ws -> Maybe (Workspace ws)
mapWSMaybe fun (LeafWS ws) = LeafWS <$> fun ws
mapWSMaybe fun (SplitWS o d m l r) = SplitWS o d m <$> (mapWSMaybe fun l) <*> (mapWSMaybe fun r)

mapWSMaybe' :: (HLWorkspace ws -> Maybe (HLWorkspace ws)) -> Workspace ws -> Maybe (Workspace ws)
mapWSMaybe' fun (LeafWS ws) = LeafWS <$> fun ws
mapWSMaybe' fun (SplitWS o d m l r) = case ((mapWSMaybe' fun l), (mapWSMaybe' fun r)) of
    (Nothing, Nothing) -> Nothing
    (ml, mr) -> Just $ SplitWS o d m (fromMaybe l ml) (fromMaybe r mr)

sameLayout :: (WSTag ws, GenericLayoutClass l (U.Zipper Seat View) ws)
           => l -> ViewSet ws
sameLayout l = ViewSet mempty (GenericLayout l)


adjustWS :: Ord ws
         => (Workspace ws -> Workspace ws) -> ws -> ViewSet ws -> ViewSet ws
adjustWS fun ws (ViewSet m l) = ViewSet (M.adjust fun ws m) l

alterWS :: Ord ws
        => (Workspace ws -> Workspace ws) -> ws -> ViewSet ws -> ViewSet ws
alterWS fun ws (ViewSet m l) =
    let empty = LeafWS $ U.Workspace l Nothing
     in ViewSet (M.alter (Just . fun . fromMaybe empty) ws m) l

adjustWS' :: Ord ws
          => (Workspace ws -> Maybe (Workspace ws)) -> ws -> ViewSet ws -> Maybe (ViewSet ws)
adjustWS' fun ws (ViewSet m l) = case fun =<< M.lookup ws m of
    Nothing -> Nothing
    Just ret -> Just $ ViewSet (M.insert ws ret m) l

mapVS :: Ord ws => (Workspace ws -> Workspace ws) -> ViewSet ws -> ViewSet ws
mapVS fun (ViewSet m l) = ViewSet (fun `fmap` m) l

getFocused :: forall ws. Maybe Seat -> Workspace ws -> Workspace ws
getFocused _ ws@(LeafWS {}) = ws
getFocused Nothing (SplitWS _ _ m first second) = case listToMaybe $ M.elems m of
    (Just FocusSecond) -> getFocused Nothing second
    _ -> getFocused Nothing first
getFocused (Just s) (SplitWS _ _ m first second) = getSub (M.lookup s m)
    where   getSub :: Maybe HLFocus -> Workspace ws
            getSub (Just FocusSecond) = getFocused (Just s) second
            getSub _ = getFocused (Just s) first

modifyFocused :: forall ws. Maybe Seat -> (Workspace ws -> Maybe (Workspace ws)) -> Workspace ws -> Maybe (Workspace ws)
modifyFocused _ fun ws@(LeafWS {}) = fun ws
modifyFocused Nothing fun (SplitWS o d m first second) = case modifyFocused Nothing fun first of
    Nothing -> Nothing
    Just ret -> Just $ SplitWS o d m ret second
modifyFocused (Just s) fun (SplitWS o d m first second) = modifySub $ M.lookup s m
    where   modifySub :: Maybe HLFocus -> Maybe (Workspace ws)
            modifySub (Just FocusSecond) = case modifyFocused (Just s) fun second of
                Nothing -> Nothing
                Just ret -> Just $ SplitWS o d m first ret
            modifySub _ = case modifyFocused (Just s) fun first of
                Nothing -> Nothing
                Just ret -> Just $ SplitWS o d m ret second

modifyFocusedC :: forall ws. Maybe Seat -> (Workspace ws -> Workspace ws) -> Workspace ws -> Workspace ws
modifyFocusedC _ _ ws@LeafWS {} = ws
modifyFocusedC s fun ws@(SplitWS o r m first second) = case flip M.lookup m =<< s of
    Just FocusSecond -> case second of
        LeafWS {} -> fun ws
        SplitWS {} -> SplitWS o r m first (modifyFocusedC s fun second)
    _ -> case first of
        LeafWS {} -> fun ws
        SplitWS {} -> SplitWS o r m (modifyFocusedC s fun first) second

changeSize :: Direction -> (Double -> Double) -> Maybe Seat -> Workspace ws -> Maybe (Workspace ws)
changeSize _ _ _ LeafWS {} = Nothing
changeSize HLRight step s (SplitWS Horizontal r m first second) = case flip M.lookup m =<< s of
    (Just FocusSecond) -> SplitWS Horizontal r m first <$> changeSize HLRight step s second
    _ -> Just $ case changeSize HLRight step s first of
        Just first' -> SplitWS Horizontal r m first' second
        Nothing -> SplitWS Horizontal (step r) m first second
changeSize HLLeft step s (SplitWS Horizontal r m first second) = case flip M.lookup m =<< s of
    (Just FocusSecond) -> Just $ case changeSize HLLeft step s second of
        Just second' -> SplitWS Horizontal r m first second'
        Nothing -> SplitWS Horizontal (step r) m first second
    _ -> SplitWS Horizontal r m <$> changeSize HLRight step s first <*> Just second
changeSize HLUp step s (SplitWS Vertical r m first second) = case flip M.lookup m =<< s of
    (Just FocusSecond) -> Just $ case changeSize HLUp step s second of
        Nothing -> SplitWS Vertical (step r) m first second
        Just second' -> SplitWS Vertical r m first second'
    _ -> SplitWS Vertical r m <$> changeSize HLUp step s first <*> Just second
changeSize HLDown step s (SplitWS Vertical r m first second) = case flip M.lookup m =<< s of
    (Just FocusSecond) -> SplitWS Vertical r m first <$> changeSize HLDown step s second
    _ -> Just $ case changeSize HLDown step s first of
        Nothing -> SplitWS Vertical (step r) m first second
        Just first' -> SplitWS Vertical r m first' second
changeSize dir step s (SplitWS o r m first second) = case flip M.lookup m =<< s of
    (Just FocusSecond) -> SplitWS o r m first <$> changeSize dir step s second
    _ -> SplitWS o r m <$> changeSize dir step s first <*> Just second

handleHLWMMessage :: Ord ws => HLWMMessage -> Maybe Seat -> ws -> ViewSet ws -> ViewSet ws
handleHLWMMessage m@HLWMSplit {} s = alterWS (fromJust . modifyFocused s (Just . splitWS m))
handleHLWMMessage SwitchOrientation s = alterWS (modifyFocusedC s switchOrientation)
handleHLWMMessage HLWMMerge s = alterWS (modifyFocusedC s mergeLeafes)
handleHLWMMessage SendOver s = alterWS (modifyFocusedC s sendOver)
    where   sendOver ws@LeafWS {} = ws
            sendOver (SplitWS o r m first second) = case flip M.lookup m =<< s of
                (Just FocusSecond) ->
                    let Just v = U.getMFocused s $ unLeafWS second
                        Just first' = modifyFocused s (Just . mapWorkspace (U.addView s v)) first
                    in case removeViewFrom v second of
                        (LeafWS (U.Workspace _ Nothing)) -> first'
                        second' -> SplitWS o r m first' second'
                _ ->
                    let Just v = U.getMFocused s $ unLeafWS first
                        Just second' = modifyFocused s (Just . mapWorkspace (U.addView s v)) second
                    in case removeViewFrom v first of
                        (LeafWS (U.Workspace _ Nothing)) -> second'
                        first' -> SplitWS o r m first' second'
handleHLWMMessage (ChangeSize dir step) s = alterWS (\ws -> fromMaybe ws $ changeSize dir step s ws)

removeViewFrom :: View -> Workspace ws -> Workspace ws
removeViewFrom v (LeafWS xs) = LeafWS $ U.rmView v xs
removeViewFrom v (SplitWS o r m first second) = 
    let first' = removeViewFrom v first
        second' = removeViewFrom v second
     in case first' of
            LeafWS (U.Workspace _ Nothing) -> second'
            _ -> case second' of
                    LeafWS (U.Workspace _ Nothing) -> first'
                    _ -> SplitWS o r m first' second'

instance Ord ws => Layouted (ViewSet ws) ws where
    {-# SPECIALISE instance Layouted (ViewSet Text) Text #-}
    getLayout _ _ = Nothing
    messageWS m s ws vs
        | Just message <- getMessage m = handleHLWMMessage message s ws vs
        | otherwise = fromMaybe vs $ adjustWS' (modifyFocused s $ mapWSMaybe modify) ws vs
            where   modify :: HLWorkspace ws -> Maybe (HLWorkspace ws)
                    modify (U.Workspace (GenericLayout l) z) = Just $ case handleMessage l s m of
                        Nothing -> U.Workspace (GenericLayout l) z
                        Just nl -> U.Workspace (GenericLayout nl) z
    broadcastWS m ws vs = fromMaybe vs $ adjustWS' (mapWSMaybe' modify) ws vs
        where   modify :: HLWorkspace ws -> Maybe (HLWorkspace ws)
                modify (U.Workspace (GenericLayout l) z) = Just $ case broadcastMessage l m of
                    Nothing -> U.Workspace (GenericLayout l) z
                    Just nl -> U.Workspace (GenericLayout nl) z
    broadcastVS m _ (ViewSet wss d) = ViewSet (fmap (\ws -> fromMaybe ws $ mapWSMaybe' modify ws) wss) d
        where   modify :: HLWorkspace ws -> Maybe (HLWorkspace ws)
                modify (U.Workspace (GenericLayout l) z) = Just $ case broadcastMessage l m of
                    Nothing -> U.Workspace (GenericLayout l) z
                    Just nl -> U.Workspace (GenericLayout nl) z

instance WSTag ws => FocusCore (ViewSet ws) ws where
    {-# SPECIALISE instance FocusCore (ViewSet Text) Text #-}
    _getFocused (ViewSet vs _) ws (Just s) = U.getFocused s . unLeafWS . getFocused (Just s) =<< M.lookup ws vs
    _getFocused (ViewSet vs _) ws Nothing = U.getFirstFocused . unLeafWS . getFocused Nothing =<< M.lookup ws vs
    _getViews (ViewSet vs _) ws = fromMaybe mempty $ case M.lookup ws vs of
        Just aws -> Just $ combineViews aws
        Nothing -> Nothing
        where   combineViews (LeafWS (U.Workspace _ z)) = fromMaybe mempty $ fmap (S.fromList . U.unZipper) z
                combineViews (SplitWS _ _ _ first second) = combineViews first <> combineViews second
    getLayouted (ViewSet m _) ws gbox = case (M.lookup ws m) of
        Just hlws -> getHLLayout (const True) hlws gbox
        Nothing -> []
        where   getHLLayout :: (Seat -> Bool) ->  Workspace ws -> WlrBox -> [(View, SSDPrio, WlrBox)]
                getHLLayout s (LeafWS (U.Workspace (GenericLayout l) (Just (U.Zipper xs)))) box =
                    let tmap fun (a, b) = (fun a, b)
                     in pureLayout l (U.Zipper (fmap (tmap (S.filter s)) xs)) ws box
                getHLLayout _ (LeafWS _) _ = mempty
                getHLLayout s (SplitWS Vertical r wsm first second) (WlrBox x y w h) =
                    let height = floor $ r * fromIntegral h
                        other = h - height
                        fSeats = S.fromList $ filter s $ map fst $ filter ((==) FocusFirst . snd) $ M.toList wsm
                        sSeats = S.fromList $ filter s $ map fst $ filter ((==) FocusSecond . snd) $ M.toList wsm
                        fS = (`S.member` fSeats)
                        sS = (`S.member` sSeats)
                     in getHLLayout fS first (WlrBox x y w height) <> getHLLayout sS second (WlrBox x (y + height) w other)
                getHLLayout s (SplitWS Horizontal r wsm first second) (WlrBox x y w h) =
                    let width = floor $ r * fromIntegral w
                        other = w - width
                        fSeats = S.fromList $ filter s $ map fst $ filter ((==) FocusFirst . snd) $ M.toList wsm
                        sSeats = S.fromList $ filter s $ map fst $ filter ((==) FocusSecond . snd) $ M.toList wsm
                        fS = (`S.member` fSeats)
                        sS = (`S.member` sSeats)
                     in getHLLayout fS first (WlrBox x y width h) <> getHLLayout sS second (WlrBox (x + width) y other h)
    _insertView ws s v vs = alterWS (fromJust . (modifyFocused s (Just . mapWorkspace (U.addView s v)))) ws vs
    _removeView ws v vs = adjustWS (removeViewFrom v) ws vs
    removeGlobal v _ = mapVS (removeViewFrom v)
    _focusView ws s v = adjustWS (\arg -> fromMaybe arg $ doFocus arg) ws
        where   doFocus (LeafWS (U.Workspace _ Nothing)) = Nothing
                doFocus (LeafWS (U.Workspace l (Just z))) = case U.zipContains v z of
                    False -> Nothing
                    True -> Just $ LeafWS (U.Workspace l (Just $ U.setFocused' s v z))
                doFocus (SplitWS o d m first second) = 
                    let doFirst = SplitWS o d (M.insert s FocusFirst m) <$> doFocus first <*> Just second
                        doSecond = SplitWS o d (M.insert s FocusSecond m) first <$> doFocus second
                     in case M.lookup s m of
                        (Just FocusSecond) -> doSecond <|> doFirst
                        _ -> doFirst <|> doSecond
    sameVS _ _ _ = False

instance WSTag ws => ListLike (ViewSet ws) ws where
    {-# SPECIALISE instance ListLike (ViewSet Text) Text #-}
    _asList (ViewSet m _) ws =
        maybe [] (join . maybeToList . fmap U.unZipper . U.wsViews . unLeafWS . getFocused Nothing) $ M.lookup ws m
    --_moveFocusLeft    :: ws -> Seat -> vs-> vs
    _moveFocusLeft ws seat vs =
        adjustWS (fromJust . modifyFocused (Just seat) (Just . mapWorkspace (U.moveLeft seat))) ws vs
    _moveFocusRight ws seat vs =
        adjustWS (fromJust . modifyFocused (Just seat) (Just . mapWorkspace (U.moveRight seat))) ws vs
    _moveFocusedLeft ws seat vs =
        adjustWS (fromJust . modifyFocused (Just seat) (Just . mapWorkspace (U.moveViewLeft seat))) ws vs
    _moveFocusedRight ws seat vs =
        adjustWS (fromJust . modifyFocused (Just seat) (Just . mapWorkspace (U.moveViewRight seat))) ws vs

