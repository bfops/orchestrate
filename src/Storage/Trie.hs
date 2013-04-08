{-# LANGUAGE NoImplicitPrelude
           #-}
module Storage.Trie( Trie (EmptyTrie, Value)
                   , fromMap
                   , mapWithKeys
                   , foldrWithKeys
                   , foldlWithKeys
                   , trie
                   , retrie
                   , triesert
                   , notrie
                   ) where

import Prelewd

import Storage.List
import Storage.Map
import Test.QuickCheck (Arbitrary (..), arbitrary)
import Text.Show

data Trie k v = EmptyTrie
              | Value v
              | Trie (Map k (Trie k v))
    deriving (Show, Eq)

instance (Ord k, Ord v) => Ord (Trie k v) where
    compare EmptyTrie EmptyTrie = EQ
    compare EmptyTrie _ = LT
    compare _ EmptyTrie = GT
    compare (Value _) (Trie _) = LT
    compare (Trie _) (Value _) = GT
    compare (Value x) (Value y) = compare x y
    compare (Trie x) (Trie y) = compare x y

instance Functor (Trie k) where
    fmap = mapWithKeys . \f _-> f

instance Ord k => Foldable (Trie k) where
    foldr f = foldrWithKeys $ \_-> f

instance Ord k => Monoid (Trie k v) where
    mempty = EmptyTrie
    mappend = flip $ foldrWithKeys trieAdd

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Trie k v) where
    arbitrary = fromMap <$> arbitrary

trieAdd :: Ord k => [k] -> v -> Trie k v -> Trie k v
trieAdd ks v t = triesert ks (Value v) t <?> t

-- | Create a trie from a map.
-- In ambiguous cases, the shortest key list will remain.
-- e.g. fromMap (fromList [([1, 2], 3), ([3, 1], 4), ([1], 2)]) == fromMap (fromList [([1], 2), ([3, 1], 4))
fromMap :: Ord k => Map [k] v -> Trie k v
fromMap = foldrWithKey trieAdd EmptyTrie

mapWithKeys :: ([k] -> a -> b) -> Trie k a -> Trie k b
mapWithKeys f = keyMap []
    where
        keyMap _ EmptyTrie = EmptyTrie
        keyMap ks (Value v) = Value $ f (reverse ks) v
        keyMap ks (Trie m) = Trie $ mapMaybeWithKey (\k t -> Just $ keyMap (k:ks) t) m

foldrWithKeys :: Ord k => ([k] -> v -> a -> a) -> a -> Trie k v -> a
foldrWithKeys f = keyFold []
    where
        keyFold _ a EmptyTrie = a
        keyFold ks a (Value v) = f (reverse ks) v a
        keyFold ks a (Trie m) = foldrWithKey (\k t a' -> keyFold (k:ks) a' t) a m

foldlWithKeys :: Ord k => ([k] -> v -> a -> a) -> a -> Trie k v -> a
foldlWithKeys f = keyFold []
    where
        keyFold _ a EmptyTrie = a
        keyFold ks a (Value v) = f (reverse ks) v a
        keyFold ks a (Trie m) = foldlWithKey (\a' k t -> keyFold (k:ks) a' t) a m

trie :: Ord k => k -> Trie k v -> Maybe (Trie k v)
trie k (Trie m) = lookup k m
trie _ _ = Nothing

retrie :: Ord k => (Trie k v -> Trie k v) -> [k] -> Trie k v -> Maybe (Trie k v)
retrie f [] t = Just $ f t
retrie f (k:ks) EmptyTrie = Trie . singleton k <$> retrie f ks EmptyTrie
retrie _ _ (Value _) = Nothing
retrie f (k:ks) (Trie m) = retrie f ks (lookup k m <?> EmptyTrie)
                       <&> \t -> Trie $ insert k t m

-- | Insert a subtrie into the trie.
-- If a subtrie already exists at these keys, it will be removed.
triesert :: Ord k
         => [k]         -- ^ Keys
         -> Trie k v    -- ^ Subtrie
         -> Trie k v    -- ^ Trie to modify
         -> Maybe (Trie k v)
triesert ks t = retrie (\_-> t) ks

-- | Replace a subtrie with the empty trie.
-- Note that a subtrie down to the keys supplied will necessarily exist in the product.
notrie :: Ord k => [k] -> Trie k v -> Maybe (Trie k v)
notrie = retrie $ \_-> EmptyTrie
