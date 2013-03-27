{-# LANGUAGE NoImplicitPrelude
           #-}
module Input ( Input (..)
             , Harmony
             , InputMap
             , fromMelody
             , fromHarmony
             , isRecord
             , isPlay
             , fromRemap
             ) where

import Prelewd

import Data.Int
import Sound.MIDI.Monad.Types
import Storage.Map

import Wrappers.Events

type Harmony = (Maybe Instrument, Int16)
type InputMap = Map (Either Button (Pitch, Instrument)) (Velocity -> Input)

data Input = Melody [Note]
           | Harmony [Harmony]
           | Record
           | Play
           | Remap InputMap

fromMelody :: Input -> Maybe [Note]
fromMelody (Melody s) = Just s
fromMelody _ = Nothing

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
