{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , Arrows
           #-}
module Logic.Memory( memory
                   ) where

import Prelewd

import Control.Stream
import Control.Stream.Input
import Data.Tuple
import Storage.Id
import Storage.List
import Storage.Map hiding (filter)
import Storage.SplitQueue

import Sound.MIDI.Monad.Types

import Input
import Types

unzipMap :: Ord k => Map k (a, b) -> (Map k a, Map k b)
unzipMap m = let (ks, vs) = unzip $ assocs m
             in (zip ks >>> fromList) *** (zip ks >>> fromList) $ unzip vs

type Song = SplitQueue (Tick, (Maybe Velocity, Note))

-- | Indicates whether to toggle play/record state, and which one to toggle
type Toggle = Maybe Bool
type TrackUpdate = ((Toggle, Tick), Chord)
type Memory = Map Track (Stream Id TrackUpdate Chord)

edge :: MonadPlus m => (a -> a -> Bool) -> a -> Stream Id (a, m b) (m b)
edge f = loop $ barr edgeFunc
    where
        edgeFunc (next, out) prev = (guard (f prev next) >> out, next)

toggle :: Num a => Maybe a -> Maybe a
toggle m = m $> Nothing <?> Just 0

timer :: Num t => (a -> Bool) -> (a -> t) -> Stream Id (a, Maybe t) b -> Stream Id a b
timer p dt s = loop timerStep Nothing
    where
        timerStep = proc (a, t) -> do
                        let t' = (dt a +) <$> t
                        b <- s -< (a, t')
                        id -< (b, if' (p a) toggle t')

-- | If the track does not exist, create it
createTrack :: Track -> Memory -> Memory
createTrack = alter (<|> Just track)

memory :: Stream Id ((Maybe (Maybe Velocity, Input), Tick), Chord) Chord
memory = loop (barr memoryFunc) mempty >>> arr concat
    where
        whichTrack mi = do
                        (v, i) <- mi
                        _ <- v
                        ((True,) <$> fromPlay i) <|> ((False,) <$> fromRecord i)

        memoryFunc ((i, dt), notes) = let toggleTrack = whichTrack i
                                      in try createTrack (snd <$> toggleTrack)
                                     >>> mapWithKey (\k s -> runId $ s $< ((perTrackInput k toggleTrack, dt), notes))
                                     >>> unzipMap

        perTrackInput k = filter (snd >>> (== k)) >>> map fst

track :: Stream Id TrackUpdate Chord
track = loop (withFstFst record >>> play) emptySplit
    where
        withFstFst s = arr (fst >>> fst) &&& s

record :: Stream Id (((Toggle, Tick), Chord), Song) Song
record = timer (fst >>> fst >>> fst >>> (== Just False)) (fst >>> fst >>> snd) $ barr recordFunc
    where
        recordFunc ((_, notes), song) t = try (append notes) t song
        append notes t q = foldr enqSplit q $ (t,) <$> notes

play :: Stream Id ((Toggle, Tick), Song) (Chord, Song)
play = timer (fst >>> fst >>> (== Just True)) (fst >>> snd)
     $ proc (a, t) -> do
            (toPlay, remain) <- barr playFunc -< (a, t)
            currentlyPlaying <- mapMaybe hold >>> map held >>> arr last >>> latch mempty >>> arr toList -< toPlay
            -- Release currently playing notes if the track stops
            stop <- edge (>) False -< (t <&> (\_-> True) <?> False, currentlyPlaying)
            id -< (toPlay <> ((Nothing,) <$> stop), remain)
    where
        playFunc (_, song) t = shiftTrack song <$> t <?> ([], reset song)
        shiftTrack song t = map2 (map snd) $ shiftWhile (fst >>> (<= t)) song
