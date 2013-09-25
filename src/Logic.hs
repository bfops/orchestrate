{-# LANGUAGE NoImplicitPrelude
           , Arrows
           #-}
module Logic ( logic
             ) where

import Summit.Control.Stream as S
import Summit.Data.Map (lookup)
import Summit.IO (writeFile, readFile)
import Summit.Prelewd

import Control.Eff as E
import Text.Read (read)
import Text.Show (show)

import Control.Stream.Util
import Sound.MIDI

import Logic.Memory
import Logic.Sequential

import Input

drumChannel :: Num a => a
drumChannel = 9

logic :: MIDI env => Stream (Eff env) (Maybe (Maybe Velocity, Input), Tick) ()
logic = proc (inpt, dt) -> do
          userPlayedNotes <- identify seqLogic -< inpt
          loadedInput <- S.lift (arr load) -< inpt
          (memPlayedNotes, memData) <- identify $ unzipMap <$> memory
                                    -< ((loadedInput, dt), userPlayedNotes)
          () <- S.lift (barr save) -< (inpt, memData)
          toPlay <- identify $ mapMaybe holdOff -< userPlayedNotes <> concat memPlayedNotes
          S.lift (arr outputNotes) -< toPlay
  where
    save (Just (Just _, Track Save t)) recorded = lookup t recorded
                                              <&> E.lift . writeFile ("track" <> show t) . show
                                              <?> pure ()
    save _ _ = pure ()

    load :: MIDI env
         => Maybe (Maybe Velocity, Input)
         -> Eff env (Maybe (Maybe Velocity, (Track, TrackCommand RecordedData)))
    load (Just (v, Track c t)) = traverse (\_-> E.lift (readFile ("track" <> show t))
                                            <&> read
                                          ) c
                             <&> \d -> Just (v, (t, d))
    load _ = pure Nothing

    outputNotes notes = traverse_ (barr sendMIDI) notes >> flush

    sendMIDI :: MIDI env => Maybe Velocity -> Note -> Eff env ()
    sendMIDI mv = mv
              <&> startNote drumChannel 0
              <?> stopNote  drumChannel 0
