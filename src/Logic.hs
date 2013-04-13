{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
module Logic ( song
             ) where

import Prelewd

import Impure

import Control.Stream
import Data.Tuple
import Sound.MIDI.Monad
import Storage.Id
import Storage.Map
import Storage.Set

import Input

import Logic.Memory

bound :: (Ord a, Bounded a) => a -> a
bound = min maxBound . max minBound

song :: Stream Id ((Maybe Velocity, Input), Tick) Song
song = updater (memory &&& noteLogic >>> loop (barr toSong) mempty) []
    where
        noteLogic = arr (fst.fst) >>> arr (map $ fromChord >>> (<?> [])) &&& harmonies

harmonies :: Stream Id (Maybe Velocity, Input) [Harmony]
harmonies = updater (barr newInputMap) mempty >>> arr (toList >>> ((Nothing, (Nothing, 0)) :))
    where
        newInputMap (v, i) = try (flip $ foldr $ addOrRemove v . set . (:[])) $ fromHarmony i
        -- decide whether to add or remove elements
        addOrRemove v s = v <&> (\_-> (s <>)) <?> (\\ s)

toSong :: (Song, ((Maybe Velocity, [Note]), [Harmony]))
       -> Map Note [Harmony]
       -> (Song, Map Note [Harmony])
toSong (sng, ((Just v, notes), hs)) hmap = let newNotes = harmonize . (Just v,) <$> notes <*> hs
                                           in ( ((0,) <$> newNotes) <> sng
                                              , foldr (`insert` hs) hmap notes
                                              )
toSong (sng, ((Nothing, notes), _)) hmap = foldr newHarmonies (sng, hmap) notes
    where
        newHarmonies note (s, m) = let (hs, m') = remove note m <?> error "double-removal"
                                   in (((0,) . harmonize (Nothing, note) <$> hs) <> s, m')

harmonize :: (Maybe Velocity, Note) -> Harmony -> (Maybe Velocity, Note)
harmonize (v, (p, i)) (dv, (inst, dp)) = ( fromIntegral . bound . try (+) dv . fromIntegral <$> v
                                         , (fromIntegral $ bound $ fromIntegral p + dp
                                           , try (\x _-> x) inst i
                                           )
                                         )
