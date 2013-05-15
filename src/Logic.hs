{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , FlexibleContexts
           , Arrows
           #-}
module Logic ( song
             ) where

import Prelewd

import Control.Stream
import Control.Stream.Input
import Data.Tuple
import Sound.MIDI.Monad
import Storage.Id
import Storage.Multimap
import Storage.Set

import Input

import Logic.Memory

import Types

song :: Stream Id (Maybe (Maybe Velocity, Input), Tick) Chord
song = updater songStep mempty
    where
        songStep = memory <&> (<>) <*> noteLogic
               >>> mapMaybe hold

        noteLogic = arr (fst >>> fst)
                >>> bind (barr $ \v i -> (v,) <$$> (Left <$$> fromChord i <|> Right <$$> fromHarmony i))
                >>> map (concatMap inputToNotes)
                >>> arr (<?> [])

        rights = bind $ right >>> \m -> try insert m mempty

        toNotes ((v, note), hs) = (set [Left note], (v, note))
                                : (toList hs <&> \h -> (set [Left note, Right h], harmonize h (v, note)))

        inputToNotes = proc (v, i) -> do
                harmonies <- rights <$> held -< (v, i)
                let notes = v <&> (,) <*> left i <&> (, harmonies) <&> toNotes
                notesOn <&> (<>) <*> notesOff -< (notes, i)

notesOn :: Stream Id (Maybe [(Set (Either Note Harmony), (Velocity, Note))], Either Note Harmony) Chord
notesOn = barr (\m _-> snd <$$> m <?> []) <&> map (map2 Just)

notesOff :: Stream Id (Maybe [(Set (Either Note Harmony), (Velocity, Note))], Either Note Harmony) Chord
notesOff = loop (barr offFunc) emptyMulti
    where
        offFunc (Nothing, off) m = map2 ((Nothing,) <$>) $ multiremove off m <?> ([], m)
        offFunc (Just ons, _) m = ([], foldr (toList *** snd >>> barr multinsert) m ons)
