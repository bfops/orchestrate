{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
-- | Map a series of keys incrementally to a value.
-- (e.g. word autocompletion).
module Storage.Trie( Trie (EmptyTrie, Value)
                   , fromMap
                   , mapWithKeys
                   , foldrWithKeys
                   , foldlWithKeys
                   , trie
                   , retrie
                   , triesert
                   , notrie
                   , Storage.Trie.test
                   ) where

import Prelewd

import Test

import Storage.List
import Storage.Map
import Text.Show

data Trie k v = EmptyTrie               -- ^ The empty Trie
              | Value v                 -- This Trie maps no keys, and simply holds a value.
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

-- | Map a function across a Trie, including the keys as input.
mapWithKeys :: ([k] -> a -> b) -> Trie k a -> Trie k b
mapWithKeys f = keyMap []
    where
        keyMap _ EmptyTrie = EmptyTrie
        keyMap ks (Value v) = Value $ f (reverse ks) v
        keyMap ks (Trie m) = Trie $ mapMaybeWithKey (\k t -> Just $ keyMap (k:ks) t) m

-- | Right-fold across a Trie, including the keys as input.
foldrWithKeys :: Ord k => ([k] -> v -> a -> a) -> a -> Trie k v -> a
foldrWithKeys f = keyFold []
    where
        keyFold _ a EmptyTrie = a
        keyFold ks a (Value v) = f (reverse ks) v a
        keyFold ks a (Trie m) = foldrWithKey (\k t a' -> keyFold (k:ks) a' t) a m

-- | Left-fold across a Trie, including the keys as input.
foldlWithKeys :: Ord k => ([k] -> v -> a -> a) -> a -> Trie k v -> a
foldlWithKeys f = keyFold []
    where
        keyFold _ a EmptyTrie = a
        keyFold ks a (Value v) = f (reverse ks) v a
        keyFold ks a (Trie m) = foldlWithKey (\a' k t -> keyFold (k:ks) a' t) a m

-- | Try using a key to move a step down a Trie.
-- O(lg(n)), where `n` is the number of keys in the Trie.
trie :: Ord k => k -> Trie k v -> Maybe (Trie k v)
trie k (Trie m) = lookup k m
trie _ _ = Nothing

-- | Alter the subtrie at a location indexed by several keys.
-- O(k*lg(n) - k*lg(k)), where `n` is the number of keys in the Trie, and `k` is the number of keys indexed.
retrie :: Ord k => (Trie k v -> Trie k v) -> [k] -> Trie k v -> Maybe (Trie k v)
retrie f [] t = Just $ f t
retrie f (k:ks) EmptyTrie = Trie . singleton k <$> retrie f ks EmptyTrie
retrie _ _ (Value _) = Nothing
retrie f (k:ks) (Trie m) = retrie f ks (lookup k m <?> EmptyTrie)
                       <&> \t -> Trie $ insert k t m

-- | Insert a subtrie into the trie.
-- If a subtrie already exists at these keys, it will be replaced.
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

test :: Test
test = $(testGroupGenerator)

trieList :: Ord k => [k] -> Maybe (Trie k v) -> Maybe (Trie k v)
trieList k mt = mt >>= \t -> foldlM (flip trie) t k

prop_foldassoc :: Trie Integer Integer -> Bool
prop_foldassoc t = foldr (+) 0 t == foldl (+) 0 t

prop_foldmap :: Trie Integer Integer -> Bool
prop_foldmap t = foldr (\x y -> 2 * x + y) 0 t == sum ((2 *) <$> t)

prop_retrie :: (Trie Integer Integer, [Integer], Trie Integer Integer) -> Bool
prop_retrie (t, k, v) = check $ retrie (\_-> v) k t
    where
        check Nothing = foldlM (flip trie) t k == Nothing
        check (Just t') = foldlM (flip trie) t' k == Just v

prop_mappend :: (Trie Integer Integer, Trie Integer Integer, Trie Integer Integer) -> Bool
prop_mappend (x, y, z) = (x <> y) <> z == x <> (y <> z)
