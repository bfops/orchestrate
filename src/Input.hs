{-# LANGUAGE NoImplicitPrelude
           #-}
module Input ( Input (..)
             , UnifiedEvent
             , Song
             , Harmony
             , InputMap
             , fromChord
             , fromHarmony
             , isRecord
             , isPlay
             , fromRemap
             ) where

import Prelewd

import Data.Int
import Sound.MIDI.Monad.Types
import Storage.Trie

import Wrappers.Events

type UnifiedEvent = Either Button Note
type Song = [(Tick, (Maybe Velocity, Note))]
type Harmony = (Maybe Instrument, Int16)
type InputMap = Trie UnifiedEvent Input

data Input = Chord [Note]
           | Harmony [Harmony]
           | Record
           | Play
           | Remap InputMap

fromChord :: Input -> Maybe [Note]
fromChord (Chord s) = Just s
fromChord _ = Nothing

fromHarmony :: Input -> Maybe [Harmony]
fromHarmony (Harmony hs) = Just hs
fromHarmony _ = Nothing

isRecord :: Input -> Bool
isRecord Record = True
isRecord _ = False

isPlay :: Input -> Bool
isPlay Play = True
isPlay _ = False

fromRemap :: Input -> Maybe InputMap
fromRemap (Remap r) = Just r
fromRemap _ = Nothing
