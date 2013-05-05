{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , FlexibleContexts
           #-}
-- | Input-output conversion helper Streams
module Control.Stream.Input( hold
                           , held
                           ) where

import Prelewd

import Impure

import Control.Stream
import Storage.Id
import Storage.Map (lookup)
import Storage.Refcount
import Storage.Set

-- | Refcount inputs and only send a release signal when they've all been released.
hold :: Ord b => Stream Id (Maybe a, b) (Maybe (Maybe a, b))
hold = loop (barr holdFunc) mempty
    where
        holdFunc (Nothing, b) cxt = let cxt' = refDelete b cxt <?> error "Released unpressed input"
                                     in (mcond (lookup b cxt' == Nothing) (Nothing, b), cxt')
        holdFunc (Just a, b) cxt = let cxt' = refInsert b cxt
                                    in (mcond (lookup b cxt' == Just 1) (Just a, b), cxt')

-- | Keep track of held inputs.
held :: Ord a => Stream Id (Maybe b, a) (Set a)
held = updater (barr $ barr $ \b -> b $> insert <?> delete) mempty
