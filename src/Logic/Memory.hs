{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
module Logic.Memory( memory
                   ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Id
import Storage.List
import Storage.Map hiding (filter)
import Storage.SplitQueue

import Sound.MIDI.Monad.Types

import Input
import Types

unzipMap :: Ord k => Map k (a, b) -> (Map k a, Map k b)
unzipMap = assocs >>> unzip >>> map unzip >>> (\(k, (a, b)) -> ((k, a), (k, b))) >>> fromLists *** fromLists
    where
        fromLists = fromList . uncurry zip

type Song = SplitQueue (Tick, (Maybe Velocity, Note))

type TrackUpdate = ((Maybe Bool, Tick), Chord)
type Memory = Map Track (Stream Id TrackUpdate Chord)

timer :: Num t => (a -> Bool) -> (a -> t) -> Stream Id (a, Maybe t) b -> Stream Id a b
timer p dt s = loop (arr fst &&& barr updateTime >>> s &&& barr state) Nothing
    where
        updateTime = dt >>> (+) >>> map
        state a = if' (p a) toggle

toggle :: Num a => Maybe a -> Maybe a
toggle m = m $> Nothing <?> Just 0

-- | If the track does not exist, create it
createTrack :: Track -> Memory -> Memory
createTrack = alter (<|> Just track)

memory :: Stream Id ((Maybe (Maybe Velocity, Input), Tick), Chord) Chord
memory = arr whichTrack
     >>> loop (barr memoryFunc >>> arr unzipMap) mempty
     >>> arr concat
    where
        whichTrack = map2 $ map2 $ \m -> do
                            (v, i) <- m
                            _ <- v
                            ((True,) <$> fromPlay i) <|> ((False,) <$> fromRecord i)

        memoryFunc :: ((Maybe (Bool, Track), Tick), Chord) -> Memory -> Map Track (Chord, Stream Id TrackUpdate Chord)
        memoryFunc ((i, dt), notes) = try createTrack (snd <$> i)
                                  >>> mapWithKey (\k s -> runId $ s $< ((perTrackInput k i, dt), notes))

        perTrackInput k = filter (snd >>> (== k)) >>> map fst

track :: Stream Id TrackUpdate Chord
track = loop (arr (fst . fst) &&& record >>> play) emptySplit

record :: Stream Id (TrackUpdate, Song) Song
record = timer (fst >>> fst >>> fst >>> (== Just False)) (fst >>> fst >>> snd) $ barr recordFunc
    where
        recordFunc ((_, notes), song) t = try (append notes) t song
        append notes t q = foldr enqSplit q $ (t,) <$> notes

play :: Stream Id ((Maybe Bool, Tick), Song) (Chord, Song)
play = timer (fst >>> fst >>> (== Just True)) (fst >>> snd) $ barr playFunc
    where
        playFunc (_, song) t = shiftTrack song <$> t <?> ([], reset song)
        shiftTrack song t = map2 (snd <$>) $ shiftWhile (fst >>> (<= t)) song
