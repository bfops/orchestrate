{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | A Multimap maps a set of keys to a single value.
-- Aside from lookup, these keys are used as one cohesive group.
module Storage.Multimap( Multimap
                       , emptyMulti
                       , multinsert
                       , multilookup
                       , multidelete
                       , multiremove
                       ) where

import Prelewd

import Data.Tuple
import Storage.Bimap
import Storage.Map
import Text.Show

-- | Assign multiple codependent keys to a single value.
data Multimap k v = Multimap (Bimap k Integer) (Map Integer v)
    deriving (Show, Eq)

-- | The empty Multimap
emptyMulti :: Ord k => Multimap k v
emptyMulti = Multimap mempty mempty

-- | `multinsert ks v m` augments `m` such that any of `ks` maps to `v`.
-- O(i + k*lg(k)), where `i` is the number of insertions, and `k` is the number of keys being inserted.
multinsert :: Ord k => [k] -> v -> Multimap k v -> Multimap k v
multinsert ks v (Multimap l r) = let i = maximum (0 : keys r) + 1
                                 in Multimap (foldr (`binsert` i) l ks) (insert i v r)

-- | Combine `multilookup` and `multidelete` without loss of efficiency.
-- O(i*lg(i) + i*k*lg(k))
multiremove :: Ord k => k -> Multimap k v -> Maybe ([v], Multimap k v)
multiremove k (Multimap l r) = do (is, l') <- biremove k l
                                  l'' <- reflect <$> foldrM bidelete (reflect l') is
                                  (vs, r') <- foldrM removes (mempty, r) is
                                  return (vs, Multimap l'' r')
    where
        removes i (vs, m) = map2 (:vs) <$> remove i m

-- Find all the values to which a key is mapped.
-- | O(lg(k) + i*lg(i))
multilookup :: Ord k => k -> Multimap k v -> [v]
multilookup k m = fst <$> multiremove k m <?> mempty

-- | Remove a key, value, and codependent keys from a Multimap.
-- O(i*lg(i) + i*k*lg(k))
multidelete :: Ord k => k -> Multimap k v -> Maybe (Multimap k v)
multidelete k m = snd <$> multiremove k m
