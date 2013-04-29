-- | Establish a many-to-many function between two sets
{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
module Storage.Bimap( Bimap
                    , reflect
                    , bilookup
                    , binsert
                    , bidelete
                    , biremove
                    ) where

import Prelewd

import Data.Tuple
import Storage.Map hiding (insert, delete)
import Storage.Set
import Text.Show

data Bimap a b = Bimap { bs :: (Map a (Set b))
                       , as :: (Map b (Set a))
                       }
    deriving (Show)

instance (Eq a, Eq b) => Eq (Bimap a b) where
    (==) = (==) `on` bs

instance (Ord a, Ord b) => Ord (Bimap a b) where
    compare = compare `on` bs

instance (Ord a, Ord b) => Monoid (Bimap a b) where
    mempty = Bimap mempty mempty
    mappend (Bimap a1 b1) (Bimap a2 b2) = Bimap (a1 <> a2) (b1 <> b2)

-- | O(1)
reflect :: Bimap a b -> Bimap b a
reflect = Bimap <$> as <*> bs

-- | O(lg(a)), where `a` is the number of as.
bilookup :: (Ord a, Ord b) => a -> Bimap a b -> Set b
bilookup a m = fst <$> biremove a m <?> mempty

-- | 
binsert :: (Ord a, Ord b) => a -> b -> Bimap a b -> Bimap a b
binsert a b m = Bimap (add b a $ bs m) (add a b $ as m)
    where
        add v = alter $ \s -> Just $ insert v <$> s <?> set [v]

-- | O(lg(a) + k*lg(b)). See `biremove`
bidelete :: (Ord a, Ord b) => a -> Bimap a b -> Maybe (Bimap a b)
bidelete a m = snd <$> biremove a m

-- | O(lg(a) + k*lg(b)), where `a` is the number of as, `b` is the number of bs, and `k` is the number of values
-- mapped by the provided key.
biremove :: (Ord a, Ord b) => a -> Bimap a b -> Maybe (Set b, Bimap a b)
biremove a m = do (b, bs') <- remove a $ bs m
                  as' <- foldrM (modify $ delete a >>> Just) (as m) b
                  return (b, Bimap bs' as')
