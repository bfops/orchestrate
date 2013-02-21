{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , MultiParamTypeClasses
           , FlexibleInstances
           , FunctionalDependencies
           , UndecidableInstances
           #-}
module Logic ( noteLogic
             ) where

import Prelewd

import Control.Stream
import Data.Int
import Sound.MIDI.Monad
import Storage.Id
import Storage.Map
import Storage.Set (Set, set)
import Subset.Num

import Input

class Propogate r a b where
    propogate :: r -> a -> b

instance Propogate r a (r, a) where
    propogate = (,)

instance (Propogate r a b, Propogate r x y) => Propogate r (a, x) (b, y) where
    propogate r = propogate r *** propogate r

noteLogic :: Stream Id (Bool, Input) [(Bool, (Tick, Note))]
noteLogic = map (arr $ fromNote &&& fromHarmony)
        >>> barr propogate
        >>> map harmonies
        >>> arr (((0,) <$$>) . toNotes)

harmonies :: Stream Id (Bool, Maybe Int16) (Set Int16)
harmonies = updater (barr newInputMap) initHarmonies >>> arr (set . keys)
    where
        initHarmonies = singleton 0 (1 :: Positive Integer)

        newInputMap (_, Nothing) m = m
        newInputMap (True, Just shift) m = insertWith (+) shift 1 m
        newInputMap (False, Just shift) m = modify (\v -> toPos $ fromPos v - 1) shift m <?> m

toNotes :: ((Bool, Maybe Note), Set Int16) -> [(Bool, Note)]
toNotes ((_, Nothing), _) = []
toNotes ((b, Just note), hs) = (b,) . adjustPitch <$> toList hs
    where
        adjustPitch dp = pitch' (fromIntegral . (dp +) . fromIntegral) note
