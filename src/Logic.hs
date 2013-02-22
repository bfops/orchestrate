{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , MultiParamTypeClasses
           , FlexibleInstances
           , FunctionalDependencies
           , UndecidableInstances
           #-}
module Logic ( Song
             , song
             ) where

import Prelewd

import Impure

import Control.Stream
import Data.Int
import Data.Tuple
import Sound.MIDI.Monad
import Storage.Id
import Storage.Map
import Subset.Num

import Input

type Song = [(Bool, (Tick, Note))]

class Propogate r a b where
    propogate :: r -> a -> b

instance Propogate r a (r, a) where
    propogate = (,)

instance (Propogate r a b, Propogate r x y) => Propogate r (a, x) (b, y) where
    propogate r = propogate r *** propogate r

song :: Stream Id ((Bool, Input), Tick) Song
song = updater (memory &&& noteLogic >>> loop (barr toSong) mempty) []
    where
        memory = arr (fst.fst) &&& record >>> playback
        noteLogic = arr (fst.fst) >>> arr (map fromMelody) &&& harmonies

record :: Stream Id (((Bool, Input), Tick), Song) Song
record = updater (barr state) (Nothing, []) >>> arr snd
    where
        state (((b, inpt), dt), notes) (Just t, sng) = let t' = iff (b && inpt == Record) Nothing $ Just $ dt + t
                                                       in (t', (map (map2 (t +)) <$> notes) <> sng)
        state (((b, inpt), _), _) (Nothing, sng) = iff (b && inpt == Record) (Just 0, []) (Nothing, sng)

playback :: Stream Id ((Bool, Input), Song) Song
playback = barr $ \(b, i) s -> guard (b && i == Play) >> s

try' :: (a -> Maybe a) -> a -> a
try' f x = f x <?> x

harmonies :: Stream Id (Bool, Input) [Int16]
harmonies = arr (map fromHarmony) >>> updater (barr newInputMap) initHarmonies >>> arr keys
    where
        initHarmonies = singleton 0 (1 :: Positive Integer)

        newInputMap (_, Nothing) m = m
        newInputMap (True, Just shifts) m = foldr (\s -> insertWith (+) s 1) m shifts
        newInputMap (False, Just shifts) m = foldr (\s -> try' $ modify (\v -> toPos $ fromPos v - 1) s) m shifts

toSong :: (Song, ((Bool, Maybe [Note]), [Int16])) -> Map Note [Int16] -> (Song, Map Note [Int16])
toSong (sng, ((_, Nothing), _)) hmap = (sng, hmap)
toSong (sng, ((True, Just notes), hs)) hmap = ( ((True,) . (0,) <$> (adjustPitch <$> notes <*> hs)) <> sng
                                              , foldr (\n -> insert n hs) hmap notes
                                              )
toSong (sng, ((False, Just notes), _)) hmap = foldr foo (sng, hmap) notes
    where
        foo note (s, m) = let (v, m') = remove note m <?> error "double-removal"
                          in (((False,) . (0,) . adjustPitch note <$> v) <> s, m')

adjustPitch :: Note -> Int16 -> Note
adjustPitch note dp = pitch' (\p -> fromIntegral $ fromIntegral p + dp) note
