{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , FlexibleContexts
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

bound :: (Ord a, Bounded a) => a -> a
bound = min maxBound . max minBound

song :: Stream Id (Maybe (Maybe Velocity, Input), Tick) Chord
song = updater songStep mempty
    where
        songStep = memory <&> (<>) <*> noteLogic
               >>> mapMaybe hold

        noteLogic = arr (fst >>> fst)
                >>> bind (barr $ \v i -> (v,) <$$> (Left <$$> fromChord i <|> Right <$$> fromHarmony i))
                >>> map (concatMap $ (maybeNote &&& (held >>> rights) >>> arr (merge >>> map toNotes)) &&& arr snd
                                 >>> notesOn <&> (<>) <*> notesOff
                        )
                >>> arr (<?> [])

        rights = arr $ bind $ right >>> \m -> m <&> (:[]) <&> set <?> mempty

        merge (x, y) = x <&> (, y)

        maybeNote = arr (map left) >>> barr (liftA2 (,))

        toNotes ((v, note), hs) = (set [Left note], (v, note))
                                : (toList hs <&> \h -> (set [Left note, Right h], harmonize (v, note) h))

notesOn :: Stream Id (Maybe [(Set (Either Note Harmony), (Velocity, Note))], Either Note Harmony) Chord
notesOn = barr (\m _-> snd <$$> m <?> []) >>> arr (map $ map2 Just)

notesOff :: Stream Id (Maybe [(Set (Either Note Harmony), (Velocity, Note))], Either Note Harmony) Chord
notesOff = loop (barr offFunc) emptyMulti
    where
        offFunc (Nothing, off) m = map2 ((Nothing,) <$>) $ multiremove off m <?> ([], m)
        offFunc (Just ons, _) m = ([], foldr (toList *** snd >>> barr multinsert) m ons)

harmonize :: (Velocity, Note) -> Harmony -> (Velocity, Note)
harmonize (v, (p, i)) (dv, (inst, dp)) = ( (fromIntegral >>> try (+) dv >>> bound >>> fromIntegral) v
                                         , (fromIntegral $ bound $ fromIntegral p + dp
                                           , try (\x _-> x) inst i
                                           )
                                         )
