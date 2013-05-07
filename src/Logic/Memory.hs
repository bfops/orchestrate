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

type TrackUpdate = ((Maybe Bool, Tick), Chord)
type Memory = Map Track (Stream Id TrackUpdate Chord)

edge :: MonadPlus m => (a -> a -> Bool) -> a -> Stream Id (a, m b) (m b)
edge f = loop $ barr edgeFunc
    where
        edgeFunc (next, out) prev = (guard (f prev next) >> out, next)

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
track = map2 (updater (barr state) Nothing)
    >>> loop (playTime &&& (recordTime >>> record) >>> play) emptySplit
    where
        state (new, dt) Nothing = new <&> (, dt)
        state (new, dt) (Just (prev, t)) = guard (new /= Just prev)
                                        >> Just (prev, t + dt)

        playTime = arr $ fst >>> fst >>> filter fst >>> map snd
        recordTime = arr $ map2 $ map2 $ filter (not . fst) >>> map snd

record :: Stream Id ((Maybe Tick, Chord), Song) Song
record = barr $ \(t, notes) -> try (append notes) t
    where
        append notes t q = foldr enqSplit q $ (t,) <$> notes

play :: Stream Id (Maybe Tick, Song) (Chord, Song)
play = proc (t, song) -> do
            (toPlay, remain) <- barr playFunc -< (song, t)
            currentlyPlaying <- mapMaybe hold >>> map held >>> arr last >>> latch mempty >>> arr toList -< toPlay
            -- Release currently playing notes if the track stops
            stop <- edge (>) False -< (t <&> (\_-> True) <?> False, currentlyPlaying)
            id -< (toPlay <> ((Nothing,) <$> stop), remain)
    where
        playFunc song t = shiftTrack song <$> t <?> ([], reset song)
        shiftTrack song t = map2 (map snd) $ shiftWhile (fst >>> (<= t)) song
