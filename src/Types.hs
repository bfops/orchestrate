{-# LANGUAGE NoImplicitPrelude
           #-}
module Types( Chord
            , Track
            ) where

import Summit.Prelewd

import Sound.MIDI

-- | A group of simultaneous on/off signals
type Chord = [(Maybe Velocity, Note)]
-- | Unique identifier for a record/playback track
type Track = Integer
