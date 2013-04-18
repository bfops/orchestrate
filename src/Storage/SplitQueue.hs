{-# LANGUAGE NoImplicitPrelude
           #-}
module Storage.SplitQueue( SplitQueue
                         , emptySplit
                         , reset
                         , current
                         , enqSplit
                         , shift
                         , shiftWhile
                         ) where

import Prelewd

import Storage.Queue
import Text.Show

data SplitQueue a = SplitQueue (Queue a) (Queue a)
    deriving(Show)

instance Functor SplitQueue where
    fmap f (SplitQueue q1 q2) = SplitQueue (f <$> q1) (f <$> q2)

emptySplit :: SplitQueue a
emptySplit = SplitQueue mempty mempty

reset :: SplitQueue a -> SplitQueue a
reset (SplitQueue q1 q2) = SplitQueue mempty (q1 <> q2)

current :: SplitQueue a -> Queue a
current (SplitQueue _ q) = q

enqSplit :: a -> SplitQueue a -> SplitQueue a
enqSplit x (SplitQueue q1 q2) = SplitQueue q1 (enq x q2)

shift :: SplitQueue a -> Maybe (SplitQueue a)
shift (SplitQueue q1 q2) = SplitQueue <$> (front q2 <&> (`enq` q1)) <*> deq q2

shiftWhile :: (a -> Bool) -> SplitQueue a -> ([a], SplitQueue a)
shiftWhile p s = recurse <$> filter p (front $ current s) <*> shift s <?> ([], s)
    where
        recurse x = shiftWhile p >>> map2 (x:)
