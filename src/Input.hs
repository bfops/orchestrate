{-# LANGUAGE NoImplicitPrelude
           #-}
module Input ( Input (..)
             , fromMelody
             , fromHarmony
             ) where

import Prelewd

import Data.Int
import Sound.MIDI.Monad.Types
import Text.Show

data Input = Melody [Note]
           | Harmony [Int16]
           | Record
           | Play
    deriving (Show, Eq, Ord)

fromMelody :: Input -> Maybe [Note]
fromMelody (Melody s) = Just s
fromMelody _ = Nothing

fromHarmony :: Input -> Maybe [Int16]
fromHarmony (Harmony hs) = Just hs
fromHarmony _ = Nothing
