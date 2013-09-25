{-# LANGUAGE NoImplicitPrelude
           , DeriveFunctor
           , DeriveFoldable
           , FlexibleInstances
           , MultiParamTypeClasses
           #-}
-- | Program-specific input types
module Input ( Input (..)
             , TrackCommand (..)
             , UnifiedEvent
             , Harmony
             , Track
             , InputMap
             , fromChord
             , fromHarmony
             , fromRemap
             , fromTrack
             , fromLoad
             , harmonize
             ) where

import Summit.Prelewd

import Data.Int
import Data.Trie
import Text.Show (Show)

import Sound.MIDI.Types

import Wrappers.Events

import Types

-- | Cap a number between its min and max.
bound :: (Ord a, Bounded a) => a -> a
bound = min maxBound . max minBound

type UnifiedEvent = Either Button Note
-- | A Harmony consists of an optional velocity shift,
-- an optional instrument, and either a static pitch or a pitch shift
type Harmony = (Maybe Int16, (Maybe Instrument, Either Pitch Int16))
type InputMap = Trie UnifiedEvent Input

data Input = Chord [Note]
           | Harmony [Harmony]
           | Remap InputMap
           | Track (TrackCommand ()) Track
    deriving (Show, Eq, Ord)

data TrackCommand a = Record
                    | Play
                    | Save
                    | Load a
    deriving (Show, Eq, Ord, Functor, Foldable)

instance Applicative f => Sequential TrackCommand f a where
  sequence (Load f) = Load <$> f
  sequence Record = pure Record
  sequence Play = pure Play
  sequence Save = pure Save

instance Applicative f => Traversable TrackCommand f a b

fromChord :: Input -> Maybe [Note]
fromChord (Chord s) = Just s
fromChord _ = Nothing

fromHarmony :: Input -> Maybe [Harmony]
fromHarmony (Harmony hs) = Just hs
fromHarmony _ = Nothing

fromTrack :: Input -> Maybe (TrackCommand (), Track)
fromTrack (Track c t) = Just (c, t)
fromTrack _ = Nothing

fromRemap :: Input -> Maybe InputMap
fromRemap (Remap r) = Just r
fromRemap _ = Nothing

fromLoad :: TrackCommand a -> Maybe a
fromLoad (Load a) = Just a
fromLoad _ = Nothing

-- | Apply a harmony to a note.
harmonize :: Harmony -> (Velocity, Note) -> (Velocity, Note)
harmonize (dv, (inst, dp)) (v, (p, i)) = ( (fromIntegral >>> try (+) dv >>> bound >>> fromIntegral) v
                                         , ( either id ((fromIntegral p +) >>> bound >>> fromIntegral) dp
                                           , inst <?> i
                                           )
                                         )
