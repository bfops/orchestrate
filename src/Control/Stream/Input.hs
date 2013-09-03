{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Input-output conversion helper Streams
module Control.Stream.Input( hold
                           , holdOff
                           , held
                           ) where

import Prelewd

import Impure

import Control.Stream
import Data.Maybe (isNothing)
import Storage.Id
import qualified Storage.Map as Map
import Storage.Refcount
import qualified Storage.Set as Set

-- | Refcount inputs and only send a release signal when they've all been released.
holdOff :: Ord b => Stream Id (Maybe a, b) (Maybe (Maybe a, b))
holdOff = loop (barr holdFunc) mempty
    where
        holdFunc (Nothing, b) cxt = refDelete b cxt
                                <&> (\cxt' ->
                                    ( mcond (isNothing $ Map.lookup b cxt')
                                            (Nothing, b)
                                    , cxt'
                                    )
                                    )
                                <?> (Nothing, cxt)
        holdFunc (Just a, b) cxt = (Just (Just a, b), refInsert b cxt)

-- | `holdOff`, with the extra functionality of only sending the first of the on signals.
hold :: Ord b => Stream Id (Maybe a, b) (Maybe (Maybe a, b))
hold = holdOff >>> bind (loop (barr holdFunc) mempty)
    where
        holdFunc (Nothing, b) cxt = (Just (Nothing, b), Map.delete b cxt <?> error "Released unpressed input")
        holdFunc (Just a, b) cxt = let cxt' = refInsert b cxt
                                   in (mcond (Map.lookup b cxt' == Just 1) (Just a, b), cxt')

-- | Keep track of held inputs. The `b` values don't matter.
held :: Ord a => Stream Id (Maybe b, a) (Set.Set a)
held = updater (barr $ barr $ \b -> b $> Set.insert <?> Set.delete) mempty
