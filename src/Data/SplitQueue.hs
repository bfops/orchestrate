{-# LANGUAGE NoImplicitPrelude
           #-}
-- | A Queue with a sliding window of focus (e.g. a playback track).
module Data.SplitQueue( SplitQueue
                         , emptySplit
                         , reset
                         , remaining
                         , enqSplit
                         , shift
                         , shiftWhile
                         ) where

import Prelewd

import Data.Queue
import Text.Show

data SplitQueue a = SplitQueue (Queue a) (Queue a)
    deriving(Show)

instance Functor SplitQueue where
    fmap f (SplitQueue q1 q2) = SplitQueue (f <$> q1) (f <$> q2)

-- | The empty SplitQueue
emptySplit :: SplitQueue a
emptySplit = SplitQueue mempty mempty

-- | Move the focus back to the beginning.
-- O(l), where l is the size of the queue before the focus.
reset :: SplitQueue a -> SplitQueue a
reset (SplitQueue q1 q2) = SplitQueue mempty (q1 <> q2)

-- | Everything after the focus.
-- O(1)
remaining :: SplitQueue a -> Queue a
remaining (SplitQueue _ q) = q

-- | Enqueue into a SplitQueue.
-- O(1)
enqSplit :: a -> SplitQueue a -> SplitQueue a
enqSplit x (SplitQueue q1 q2) = SplitQueue q1 (enq x q2)

-- | Move the focus forward by one element.
-- O(1)
shift :: SplitQueue a -> Maybe (SplitQueue a)
shift (SplitQueue q1 q2) = SplitQueue <$> (front q2 <&> (`enq` q1)) <*> deq q2

-- | Move the focus while a predicate holds over the elements.
shiftWhile :: (a -> Bool) -> SplitQueue a -> ([a], SplitQueue a)
shiftWhile p s = recurse <$> filter p (front $ remaining s) <*> shift s <?> ([], s)
    where
        recurse x = shiftWhile p >>> map2 (x:)
