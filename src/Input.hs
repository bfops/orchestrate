{-# LANGUAGE NoImplicitPrelude
           #-}
module Input ( Input (..)
             , fromNote
             , fromHarmony
             ) where

import Prelewd

import Data.Int
import Sound.MIDI.Monad.Types
import Text.Show

data Input = NoteKey Note
           | Harmony Int16
           | Record
           | Play
    deriving (Show, Eq, Ord)

fromNote :: Input -> Maybe Note
fromNote (NoteKey note) = Just note
fromNote _ = Nothing

fromHarmony :: Input -> Maybe Int16
fromHarmony (Harmony shift) = Just shift
fromHarmony _ = Nothing
