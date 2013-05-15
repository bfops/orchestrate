{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Program-specific input types
module Input ( Input (..)
             , UnifiedEvent
             , Harmony
             , Track
             , InputMap
             , fromChord
             , fromHarmony
             , fromRecord
             , fromPlay
             , fromRemap
             , harmonize
             ) where

import Prelewd

import Data.Int
import Sound.MIDI.Monad.Types
import Storage.Trie

import Text.Show

import Wrappers.Events

import Types

bound :: (Ord a, Bounded a) => a -> a
bound = min maxBound . max minBound

type UnifiedEvent = Either Button Note
-- | A Harmony consists of an optional velocity shift,
-- an optional instrument, and a pitch shift.
type Harmony = (Maybe Int16, (Maybe Instrument, Int16))
type InputMap = Trie UnifiedEvent Input

data Input = Chord [Note]
           | Harmony [Harmony]
           | Record Track
           | Play Track
           | Remap InputMap
    deriving (Show, Eq, Ord)

fromChord :: Input -> Maybe [Note]
fromChord (Chord s) = Just s
fromChord _ = Nothing

fromHarmony :: Input -> Maybe [Harmony]
fromHarmony (Harmony hs) = Just hs
fromHarmony _ = Nothing

fromRecord :: Input -> Maybe Track
fromRecord (Record t) = Just t
fromRecord _ = Nothing

fromPlay :: Input -> Maybe Track
fromPlay (Play t) = Just t
fromPlay _ = Nothing

fromRemap :: Input -> Maybe InputMap
fromRemap (Remap r) = Just r
fromRemap _ = Nothing

harmonize :: Harmony -> (Velocity, Note) -> (Velocity, Note)
harmonize (dv, (inst, dp)) (v, (p, i)) = ( (fromIntegral >>> try (+) dv >>> bound >>> fromIntegral) v
                                         , ( ((fromIntegral p +) >>> bound >>> fromIntegral) dp
                                           , try (\x _-> x) inst i
                                           )
                                         )
