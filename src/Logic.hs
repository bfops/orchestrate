{-# LANGUAGE NoImplicitPrelude
           , Arrows
           #-}
module Logic ( logic
             ) where

import Summit.Control.Stream
import Summit.Data.Map (lookup)
import Summit.IO (writeFile, readFile)
import Summit.Prelewd

import Text.Read (read)
import Text.Show (show)

import Control.Stream.Util
import Sound.MIDI.Monad

import Logic.Memory
import Logic.Sequential

import Input

drumChannel :: Num a => a
drumChannel = 9

logic :: Stream MIDI (Maybe (Maybe Velocity, Input), Tick) ()
logic = proc (inpt, dt) -> do
          userPlayedNotes <- identify seqLogic -< inpt
          loadedInput <- lift (arr load) -< inpt
          (memPlayedNotes, memData) <- identify $ unzipMap <$> memory
                                    -< ((loadedInput, dt), userPlayedNotes)
          () <- lift (barr save) -< (inpt, memData)
          toPlay <- identify $ mapMaybe holdOff -< userPlayedNotes <> concat memPlayedNotes
          lift (arr outputNotes) -< toPlay
  where
    save (Just (Just _, Track Save t)) recorded = lookup t recorded
                                              <&> (\trck -> ioMIDI
                                                          $ \_-> writeFile ("track" <> show t)
                                                                           (show trck))
                                              <?> pure ()
    save _ _ = pure ()

    load :: Maybe (Maybe Velocity, Input) -> MIDI (Maybe (Maybe Velocity, (Track, TrackCommand RecordedData)))
    load (Just (v, Track c t)) = traverse (\_-> ioMIDI $ \_-> readFile ("track" <> show t)
                                            <&> read
                                          ) c
                             <&> \d -> Just (v, (t, d))
    load _ = pure Nothing

    outputNotes notes = traverse_ (barr sendMIDI) notes >> flush

    sendMIDI :: Maybe Velocity -> Note -> MIDI ()
    sendMIDI mv = mv
              <&> startNote drumChannel 0
              <?> stopNote  drumChannel 0
