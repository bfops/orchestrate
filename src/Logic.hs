{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , Arrows
           , TemplateHaskell
           #-}
module Logic ( song
             , Logic.test
             ) where

import Prelewd

import Test

import Control.Stream
import Control.Stream.Input
import Data.Tuple
import Sound.MIDI.Monad
import Storage.Id
import Storage.List (take)
import Storage.Multimap
import Storage.Set

import Logic.Memory

import Input
import Types

song :: Stream Id (Maybe (Maybe Velocity, Input), Tick) Chord
song = updater songStep mempty
    where
        songStep = memory <&> (<>) <*> noteLogic
               >>> mapMaybe holdOff

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

test :: Test
test = $(testGroupGenerator)

prop_notes :: [(Velocity, Set Note)] -> Result
prop_notes = streamTest song $ \notes -> do
                        (v, chord) <- toList <$$> take 16 notes
                        [ ioPair (Just v) chord, ioPair Nothing chord ]
    where
        -- input to expected output pairing for the stream
        ioPair v chord = ( (Just (v, Chord chord), 0)
                         , Id $ (v,) <$> chord
                         )

prop_harmony :: [(Velocity, ([Harmony], [Note]))] -> Result
prop_harmony = streamTest (set <$> song)
             $ preprocess
           >>> \inputs -> do
                    (v, (harmonies, notes)) <- inputs
                    let outNotes = set $ harmonize <$> harmonies <*> ((v,) <$> notes)
                        setNotes = (v,) <$> set notes
                    ioPair <$> [ ((Just v, Harmony harmonies), mempty)
                               , ((Just v, Chord notes), map2 Just <$> setNotes <> outNotes)
                               , ((Nothing, Harmony harmonies), unheld outNotes $ setNotes)
                               , ((Nothing, Chord notes), off <$> setNotes)
                               ]
    where
        reduceInput :: Ord a => [a] -> [a]
        reduceInput = take 16 >>> set >>> toList

        preprocess l = (reduceInput *** reduceInput) <$$> take 16 l
        ioPair = (Just >>> (, 0)) *** Id

        off = map2 $ \_-> Nothing
        unheld = (\\) `on` map off
