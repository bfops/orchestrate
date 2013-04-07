{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
module Logic ( Song
             , song
             ) where

import Prelewd

import Impure

import Control.Stream
import Data.Tuple
import Sound.MIDI.Monad
import Storage.Id
import Storage.Map
import Subset.Num

import Input

import Logic.Memory

song :: Stream Id ((Maybe Velocity, Input), Tick) Song
song = updater (memory &&& noteLogic >>> loop (barr toSong) mempty) []
    where
        noteLogic = arr (fst.fst) >>> arr (map $ fromChord >>> (<?> [])) &&& harmonies

try' :: (a -> Maybe a) -> a -> a
try' f x = f x <?> x

harmonies :: Stream Id (Maybe Velocity, Input) [Harmony]
harmonies = arr (map fromHarmony) >>> updater (barr newInputMap) initHarmonies >>> arr keys
    where
        initHarmonies = singleton (Nothing, 0) (1 :: Positive Integer)

        newInputMap (_, Nothing) m = m
        newInputMap (Just _, Just shifts) m = foldr (\s -> insertWith (+) s 1) m shifts
        newInputMap (Nothing, Just shifts) m = foldr (try' . modify (\v -> toPos $ fromPos v - 1)) m shifts

toSong :: (Song, ((Maybe Velocity, [(Pitch, Instrument)]), [Harmony]))
       -> Map (Pitch, Instrument) [Harmony]
       -> (Song, Map (Pitch, Instrument) [Harmony])
toSong (sng, ((Just v, notes), hs)) hmap = let newNotes = harmonize <$> notes <*> hs
                                           in ( ((0,) . (Just v,) <$> newNotes) <> sng
                                              , foldr (`insert` hs) hmap notes
                                              )
toSong (sng, ((Nothing, notes), _)) hmap = foldr newHarmonies (sng, hmap) notes
    where
        newHarmonies note (s, m) = let (hs, m') = remove note m <?> error "double-removal"
                                   in (((0,) . (Nothing,) . harmonize note <$> hs) <> s, m')

harmonize :: (Pitch, Instrument) -> Harmony -> (Pitch, Instrument)
harmonize (p, i) (inst, dp) = (fromIntegral $ fromIntegral p + dp, try (\x _-> x) inst i)
