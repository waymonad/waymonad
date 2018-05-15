-----------------------------------------------------------------------------
-- |
-- Module      :  SymTab
-- Copyright   :  2000-2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  All
--
-- Symbol Table, based on index trees using a hash on the key.
--   Keys are always Strings.  Stored values can be any type.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.SymTab
  ( SymTab
  , emptyST
  , insertST
  , deleteST
  , lookupST
  , definedST
  , flattenST
  , IndTree
  ) where

-- | Symbol Table.  Stored values are polymorphic, but the keys are
--   always strings.
type SymTab v = IndTree [(String,v)]

emptyST   :: SymTab v
insertST  :: (String,v) -> SymTab v -> SymTab v
deleteST  :: String -> SymTab v -> SymTab v
lookupST  :: String -> SymTab v -> Maybe v
definedST :: String -> SymTab v -> Bool
flattenST :: SymTab v -> [v]

emptyST           = itgen maxHash []
insertST (s,v) ss = itiap (hash s) ((s,v):)    ss id
deleteST  s    ss = itiap (hash s) (filter ((/=s).fst)) ss id
lookupST  s    ss = let vs = filter ((==s).fst) ((itind (hash s)) ss)
                    in if null vs then Nothing
                       else (Just . snd . head) vs
definedST s    ss = let vs = filter ((==s).fst) ((itind (hash s)) ss)
                    in (not . null) vs
flattenST      ss = itfold (map snd) (++) ss


----
-- | Index Trees (storing indexes at nodes).

data IndTree t = Leaf t | Fork Int (IndTree t) (IndTree t)
     deriving Show

itgen :: Int -> a -> IndTree a
itgen 1 x = Leaf x
itgen n x =
  let n' = n `div` 2
  in Fork n' (itgen n' x) (itgen (n-n') x)

itiap :: --Eval a =>
         Int -> (a->a) -> IndTree a -> (IndTree a -> b) -> b
itiap _ f (Leaf x)       k = let fx = f x in {-seq fx-} (k (Leaf fx))
itiap i f (Fork n lt rt) k =
  if i<n then
       itiap i f lt $ \lt' -> k (Fork n lt' rt)
  else itiap (i-n) f rt $ \rt' -> k (Fork n lt rt')

itind :: Int -> IndTree a -> a
itind _ (Leaf x) = x
itind i (Fork n lt rt) = if i<n then itind i lt else itind (i-n) rt

itfold :: (a->b) -> (b->b->b) -> IndTree a -> b
itfold leaf _fork (Leaf x) = leaf x
itfold leaf  fork (Fork _ l r) = fork (itfold leaf fork l) (itfold leaf fork r)

----
-- Hash values

maxHash :: Int -- should be prime
maxHash = 101

class Hashable a where
    hashWithMax :: Int -> a -> Int
    hash        :: a -> Int
    hash = hashWithMax maxHash

instance Enum a => Hashable [a] where
    hashWithMax m = h 0
        where h a []     = a
              h a (c:cs) = h ((17*(fromEnum c)+19*a)`rem`m) cs

----
