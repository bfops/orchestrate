{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , Arrows
           , TemplateHaskell
           #-}
-- | Logic components with obvious updating state.
module Logic.Memory( memory
                   , unzipMap
                   , RecordedData
                   , Logic.Memory.test
                   ) where

import Summit.Control.Stream
import Summit.Data.Id
import Summit.Data.List as L (unzip, zip, last, scanl1)
import Summit.Data.Map as M (Map, alter, mapWithKey, assocs, fromList)
import Summit.Data.Set (set, insert, (\\))
import Summit.Impure (error)
import Summit.Prelewd as P hiding ((!))
import Summit.Subset.Num
import Summit.Test

import Control.Stream.Util
import Data.Tuple
import Data.Maybe (isJust)
import Data.KVP (KVP (..), kvp)
import Data.Vector as V (Vector, snoc, slice, length, (!), fromList)
import Data.Traversable (mapAccumL)
import Text.Show

import Sound.MIDI

import Input
import Types

instance ResultEq a => ResultEq (Vector a) where
  (==?) = (==?) `on` toList

-- | Stream that produces the previous value it received.
-- Iff Nothing was received, produce a default value.
previousJust :: a       -- ^ Default value
             -> Stream Id (Maybe a) a
previousJust v0 = previous Nothing <&> (<?> v0)

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
edge f b0 a0 = map2 (withPrev a0 (barr f))
           <&> \(sendThrough, out) -> iff sendThrough out b0

-- | `edge` using `mzero` as the default value.
medge :: MonadPlus m => (a -> a -> Bool) -> a -> Stream Id (a, m b) (m b)
medge f = edge f mzero

unzipMap :: Ord k => Map k (a, b) -> (Map k a, Map k b)
unzipMap m = let (ks, vs) = unzip $ assocs m
             in (L.zip ks >>> M.fromList) *** (L.zip ks >>> M.fromList) $ unzip vs

-- This follows a more general form: f (g a b) -> g (f a) (f b)
unzipMapM :: (Functor m, Sequential (Map k) m (a, b), Ord k)
          => Map k (m (a, b)) -> m (Map k a, Map k b)
unzipMapM m = unzipMap <$> sequence m

type TrackUpdate = ((Maybe (TrackCommand RecordedData), Tick), Chord)
type Memory = Map Track (Stream Id TrackUpdate (Chord, RecordedData))
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

-- | Sums up the Justs since the last Nothing.
timer :: Num a => Stream Id (Maybe a) (Maybe a)
timer = folds (barr timerFunc) Nothing
  where
    timerFunc current prev = (current <&> (+) <*> prev) <|> current

-- | If the track does not exist, create it
createTrack :: Track -> Memory -> Memory
createTrack = alter (<|> Just track)

type PossibleInput = Maybe (Maybe Velocity, (Track, TrackCommand RecordedData))

memory :: Stream Id ((PossibleInput, Tick), Chord) (Map Track (Chord, RecordedData))
memory = loop (lift $ barr updateMemory) mempty
    where
        whichTrack :: PossibleInput -> Maybe (Track, TrackCommand RecordedData)
        whichTrack mi = do
                        (v, i) <- mi
                        v $> i

        updateMemory :: ((PossibleInput, Tick), Chord) -> Memory -> Id (Map Track (Chord, RecordedData), Memory)
        updateMemory ((i, dt), notes) = let toggleTrack = whichTrack i
                                        in try createTrack (fst <$> toggleTrack)
                                       >>> mapWithKey (\k s -> s $< ((perTrackInput k toggleTrack, dt), notes))
                                       >>> sequence
                                       >>> map unzipMap

        perTrackInput :: Track -> Maybe (Track, TrackCommand RecordedData) -> Maybe (TrackCommand RecordedData)
        perTrackInput trck = filter (fst >>> (== trck)) >>> map snd

track :: Stream Id TrackUpdate (Chord, RecordedData)
track = proc ((stateSwitch, dt), notes) -> do
            (playState, recordState) <- identify (playUpdate &&& recordUpdate)
                                     -< (stateSwitch, dt)
            let tryLoad = stateSwitch >>= fromLoad
            recorded <- record -< ((recordState, notes), tryLoad)
            toPlay <- identify play -< (playState, recorded)
            id -< (toPlay, recorded)
    where
        recordUpdate = map2 (toggleOn (Just Record) False) >>> barr mcond
        playUpdate = map2 (toggleOn (Just Play) False) >>> barr mcond
        toggleOn v = folds $ barr $ \x -> if' (x == v) not

record :: Stream Id ((Maybe Tick, Chord), Maybe RecordedData) RecordedData
record = map2 (map2 timer)
     >>> folds updateRecorded mempty
  where
      updateRecorded = proc (((t, notes), loaded), prev) -> do
                next <- tryResetRecording -< (isJust t, loaded <?> prev)
                id -< try (append notes) t next

      tryResetRecording = medge (>=) False

      append notes t v = if V.length v >= 0x7FFFFFFF
                         then error "Ran out of space for recording data."
                         else snoc v $ KVP t notes

play :: Stream Id (Maybe Tick, RecordedData) Chord
play = map2 withPreviousTime
   >>> proc ((tPrev, tCurrent), song) -> do
      notesToPlay <- mapMaybe holdOff
                  -< newPlayNotes song tPrev <$> tCurrent <?> []
      notesToStop <- arr isJust *** currentlyPlaying
                 >>> releaseOnFallingEdge
                  -< (tCurrent, notesToPlay)
      id -< notesToPlay <> ((Nothing,) <$> notesToStop)
    where
      releaseOnFallingEdge = medge (>) False

      withPreviousTime = timer >>> previousJust 0 &&& id

      currentlyPlaying = map held
                     <&> last
                     >>> latch mempty
                     <&> toList

      newPlayNotes song t0 t = concatMap value $ inRange (kvp t0) (kvp t) song

test :: Test
test = $(testGroupGenerator)

prop_record :: [(Positive Tick, Chord)] -> Result
prop_record = map (map2 fromPos)
          >>> mapAccumL ioPair (0, mempty)
          >>> snd
          >>> streamTestEq record
  where
    ioPair (t, recorded) (dt, chord) = let
        t' = t + dt
        recorded' = snoc recorded (KVP t' chord)
     in ((t', recorded'), (((Just dt, chord), Nothing), Id recorded'))

keysum :: Num a => [(Positive a, b)] -> [KVP a b]
keysum = transformFst (map fromPos >>> scanl1 (+)) >>> map (barr KVP)
  where
    transformFst f = unzip >>> map2 f >>> barr L.zip

prop_play :: ([(Positive Tick, Chord)], [Positive Tick]) -> Result
prop_play = (keysum >>> V.fromList) *** map fromPos
        >>> ioPairs
        >>> streamTest (set <$> play)
  where
    isSubsetOf a b = null (a \\ b)
    ioPairs (recorded, queries) = snd $ mapAccumL (ioPair recorded) 0 queries
    ioPair v t dt = let t' = t + dt
                        expectedOutput = set
                                       $ concatMap value
                                       $ inRange (kvp t) (kvp t') v
                        -- Button-holding logic can reduce the number of signals
                        -- passed through, so we expected the output to be a
                        -- subset of the expected output.
                        expectation out = if runId out `isSubsetOf` expectedOutput
                                          then succeeded
                                          else failed
                    in (t', ((Just dt, v), expectation))

prop_inRange :: ([(Positive Integer, Integer)], (Integer, Integer)) -> Result
prop_inRange = (keysum *** kvp *** kvp)
           >>> \(v, (l, h)) -> inRange l h (V.fromList v)
                           ==? filter ((> l) <&> (&&) <*> (<= h)) v

toSortedVector :: Ord a => [a] -> Vector a
toSortedVector = set >>> toList >>> V.fromList

prop_bsearch_less :: (Integer, [Integer]) -> Result
prop_bsearch_less = barr (:)
                >>> toSortedVector
                >>> \sortedInputs -> let l = sortedInputs ! 0
                                     in liftBool (bsearch (l - 1) sortedInputs == -1)

prop_bsearch_greater :: (Integer, [Integer]) -> Result
prop_bsearch_greater = barr (:)
                   >>> toSortedVector
                   >>> \sortedInputs -> let lastIndex = V.length sortedInputs - 1
                                            h = sortedInputs ! lastIndex
                                        in liftBool $ bsearch (h + 1) sortedInputs == lastIndex

data IndexedIncreasingList a = IndexedIncreasingList [a] Integer
  deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (IndexedIncreasingList a) where
  arbitrary = do l <- insert <$> arbitrary <*> arbitrary <&> toList
                 i <- choose (0, P.length l - 1)
                 return $ IndexedIncreasingList l i

prop_bsearch_found :: IndexedIncreasingList Integer -> Result
prop_bsearch_found = toTuple
                 >>> V.fromList *** fromInteger
                 >>> \(sortedInputs, i) -> liftBool $ bsearch (sortedInputs ! i) sortedInputs == i
  where
    toTuple (IndexedIncreasingList l i) = (l, i)
