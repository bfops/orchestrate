{-# LANGUAGE NoImplicitPrelude
           #-}
module Data.KVP( KVP (..)
                  , kvp
                  ) where

import Summit.Prelewd

import Summit.Impure
import Summit.Test

import Text.Show

data KVP k v = KVP { key    :: k
                   , value  :: v
                   }
    deriving (Show)

instance Eq k => Eq (KVP k v) where
    (==) = (==) `on` key

instance Ord k => Ord (KVP k v) where
    compare = compare `on` key

instance (Eq k, Show k, Show v) => ResultEq (KVP k v)

-- | KVP with undefined value. Can be used for lookups and other
-- value-independent operations.
kvp :: k -> KVP k v
kvp k = KVP k undefined
