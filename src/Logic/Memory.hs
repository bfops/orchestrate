{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , Arrows
           #-}
module Logic.Memory( memory
                   ) where

import Prelewd hiding ((!))

import Impure (error)

import Control.Stream
import Control.Stream.Input
import Data.Tuple
import Data.Vector as V (Vector, snoc, slice, length, (!))
import Storage.Id
import Storage.KVP (KVP (..), kvp)
import Storage.List as L (unzip, zip, last)
import Storage.Map as M (Map, alter, mapWithKey, assocs, fromList)

import Sound.MIDI.Monad.Types

import Input
import Types

isJust :: Maybe a -> Bool
isJust m = m <&> (\_-> True) <?> False

-- | Stream that produces the previous value it received.
previous :: a -> Stream Id a a
previous = loop $ arr swap

-- | Stream that produces the previous value it received.
-- Iff Nothing was received, produce a default value.
previousJust :: a       -- ^ Default value
             -> Stream Id (Maybe a) a
previousJust v0 = previous (Just v0) >>> arr (<?> v0)

withPrev :: a -> Stream Id (a, a) b -> Stream Id a b
withPrev a0 s = previous a0 &&& id >>> s

-- | Produce a Stream taking tuples of state values and corresponding outputs,
-- and filter the outputs using a predicate on the state transitions.
edge :: (a -> a -> Bool)    -- ^ Predicate to determine which values to send.
                            -- The first parameter is the previous state; the
                            -- second parameter is the current state.
     -> b                   -- ^ Value to be sent when the predicate is False.
     -> a                   -- ^ Initial state to use for the predicate.
     -> Stream Id (a, b) b
edge f b0 a0 = map2 (withPrev a0 $ barr f)
           <&> \(sendThrough, out) -> iff sendThrough out b0

-- | `edge` using `mzero` as the default value.
medge :: MonadPlus m => (a -> a -> Bool) -> a -> Stream Id (a, m b) (m b)
medge f = edge f mzero

unzipMap :: Ord k => Map k (a, b) -> (Map k a, Map k b)
unzipMap m = let (ks, vs) = unzip $ assocs m
             in (L.zip ks >>> M.fromList) *** (L.zip ks >>> M.fromList) $ unzip vs

type TrackUpdate = ((Maybe Bool, Tick), Chord)
type Memory = Map Track (Stream Id TrackUpdate Chord)
-- N.B. Vector is indexed by Int; can only hold 2^32 elements.
type RecordedData = Vector (KVP Tick Chord)

-- | Slice a vector between two indices, including the lower index, but not
-- the upper one.
sliceRange :: Int -> Int -> Vector a -> Vector a
sliceRange l h v = guard (h > l) >> slice l (h - l) v

-- | Search a sorted vector for an element. If the vector contains the element,
-- its index is returned.
-- Otherwise, return the index of the last element smaller than that provided.
bsearch :: Ord a => a -> Vector a -> Int
bsearch a = bsearch'
    where
        bsearch' v = if null v
                     then -1
                     else let guess = div (V.length v) 2
                              (l, h) = if a < (v ! guess)
                                       then (0, guess)
                                       else (guess + 1, V.length v)
                          in if a == (v!guess)
                             then guess
                             else l + bsearch' (sliceRange l h v)

-- | Get a series of elements in a certain range from a sorted vector.
-- inRange l h v returns a list of elements, in ascending order, greater than l
-- and not exceeding h.
-- O(lg(n))
inRange :: Ord a => a -> a -> Vector a -> [a]
inRange l h v = let
                  il = 1 + bsearch l v
                  ih = 1 + bsearch h v
                in toList $ sliceRange il ih v

accumulateTime :: Stream Id (Maybe Tick) (Maybe Tick)
accumulateTime = updater (barr addJust) (Just 0)
  where
    addJust x y = liftA2 (+) x y <|> x

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
track = proc ((stateSwitch, dt), notes) -> do
            (ps, rs) <- playUpdate &&& recordUpdate -< (stateSwitch, dt)
            r <- record -< (rs, notes)
            play -< (ps, r)
    where
        recordUpdate = map2 (toggleOn (Just False) False) >>> barr mcond
        playUpdate = map2 (toggleOn (Just True) False) >>> barr mcond
        toggleOn v = updater $ barr $ \x -> if' (x == v) not

record :: Stream Id (Maybe Tick, Chord) RecordedData
record = map2 accumulateTime
     >>> updater (reset >>> barr appendSong) mempty
  where
      reset = proc ((t, notes), song) -> do
                song' <- medge (>=) False -< (isJust t, song)
                id -< ((notes, t), song')

      appendSong = barr $ try . append
      append notes t v = if V.length v >= 0x7FFFFFFF
                         then error "Ran out of space for recording data."
                         else snoc v $ KVP t notes

play :: Stream Id (Maybe Tick, RecordedData) Chord
play = map2 (accumulateTime >>> previousJust 0 &&& id)
   >>> proc ((t0, t), song) -> do
      toPlay <- barr playFunc >>> mapMaybe hold -< (song, (t0, t))
      current <- currentlyPlaying >>> arr toList -< toPlay
      -- Release currently playing notes if the track stops
      toStop <- fallingEdge -< (isJust t, current)
      id -< toPlay <> ((Nothing,) <$> toStop)
    where
      fallingEdge = medge (>) False
      currentlyPlaying = map held >>> arr last >>> latch mempty
      playFunc song (t0, t) = newlyPlayed song t0 <$> t <?> []
      newlyPlayed song t0 t = concatMap value $ inRange (kvp t0) (kvp t) song
