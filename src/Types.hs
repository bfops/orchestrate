{-# LANGUAGE NoImplicitPrelude
           #-}
module Types( Chord
            , Track
            ) where

import Prelewd

import Sound.MIDI.Monad

type Chord = [(Maybe Velocity, Note)]
type Track = Integer
