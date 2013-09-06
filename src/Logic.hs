{-# LANGUAGE NoImplicitPrelude
           , Arrows
           #-}
module Logic ( logic
             ) where

import Summit.Control.Stream
import Summit.Prelewd

import Control.Stream.Util
import Sound.MIDI.Monad

import Logic.Memory
import Logic.Sequential

import Input

drumChannel :: Num a => a
drumChannel = 9

logic :: Stream MIDI (Maybe (Maybe Velocity, Input), Tick) ()
logic = lift $ proc (inpt, dt) -> do
                userPlayedNotes <- seqLogic -< inpt
                memData <- memory -< ((inpt, dt), userPlayedNotes)
                toPlay <- mapMaybe holdOff -< userPlayedNotes <> memData
                id -< outputNotes toPlay
  where
    outputNotes notes = traverse_ (barr sendMIDI) notes >> flush

    sendMIDI :: Maybe Velocity -> Note -> MIDI ()
    sendMIDI mv = mv
              <&> startNote drumChannel 0
              <?> stopNote  drumChannel 0
