module Trie (test) where

import Prelewd

import Test

import Storage.Trie

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
