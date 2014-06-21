-- | This module defines data structures and a basic interface for keeping track of parallel,
-- independent tape recorder style tracks. 
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module TrackMemory ( TrackMemory
                   , TrackStates
                   , TrackOutput (..)
                   , track
                   , trackData
                   , toRelease
                   , startRecording
                   , stopRecording
                   , record
                   , recordTrack
                   , startPlaying
                   , stopPlaying
                   , playSome
                   , initialTrack
                   ) where

import Prelude ()
import BasicPrelude

import Control.Lens as Lens
import Data.Foldable as Foldable
import Data.HashMap.Strict as HashMap
import Data.OpenUnion
import Data.Refcount
import Data.Vector as Vector
import Sound.MIDI.Types

import Input

data TrackOutput
    = RestOutput Tick
    | NoteOutput Note (Maybe Velocity)
  deriving (Show, Read)

-- | For keeping track of notes that have been started, but not stopped.
type NotesHeld = Refcount Note

data Stopped
    = Stopped
      { _trackData'Stopped :: Vector TrackOutput
      }
  deriving (Show, Typeable)

data Recording
    = Recording
      { _trackData'Recording :: Vector TrackOutput
      , _toRelease'Recording :: NotesHeld
      }
  deriving (Show, Typeable)

data Playing
    = Playing
      { _trackData'Playing :: Vector TrackOutput
      , _idx :: Int -- ^ the index of the play head in `trackData`
      , _leftoverT :: Tick -- ^ time is quantized via `TrackOutput`s; this is the amount of unresolved "leftover" time that wasn't prop
      , _toRelease'Playing :: NotesHeld
      , _loopBehavior :: LoopBehavior
      }
  deriving (Show, Typeable)

-- | memory bank of parallel tracks, identified by a `TracknNumber`
type TrackMemory = HashMap TrackNumber (Union TrackStates)

-- datatypes denoting track states
type TrackStates = '[Stopped, Recording, Playing]

$(makeLenses ''Stopped)
$(makeLenses ''Recording)
$(makeLenses ''Playing)

trackData :: Lens' (Union TrackStates) (Vector TrackOutput)
trackData = lens view0 set0
  where
    view0
      =  view trackData'Stopped
      @> view trackData'Recording
      @> view trackData'Playing
      @> typesExhausted

    set0 t v =
      (  liftUnion . set trackData'Stopped v
      @> liftUnion . set trackData'Recording v
      @> liftUnion . set trackData'Playing v
      @> typesExhausted
      ) t

-- | `foldl'` with a different argument order.
forLoop :: Foldable t => t a -> (b -> a -> b) -> b -> b
forLoop t f b = Foldable.foldl' f b t
{-# INLINE forLoop #-}

record :: [TrackOutput] -> TrackMemory -> TrackMemory
record outputs = fmap $ reUnion . recordTrack outputs @> reUnion

playSome :: Tick -> TrackMemory -> ([(Note, Maybe Velocity)], TrackMemory)
playSome dt mem =
    let (outputs, mem') =
          BasicPrelude.unzip $
          fmap (\(t, (o, d)) -> (o, (t, d))) $
          HashMap.toList $
          (playSomeTrack dt @> ([],) . reUnion) <$> mem
    in (Foldable.concat outputs, HashMap.fromList mem')

toRelease :: Lens' (Union '[Playing, Recording]) NotesHeld
toRelease = lens view0 set0
  where
    view0
      =  view toRelease'Recording
      @> view toRelease'Playing
      @> typesExhausted

    set0 t v =
      (  liftUnion . set toRelease'Recording v
      @> liftUnion . set toRelease'Playing v
      @> typesExhausted
      ) t

noteOutput :: TrackOutput -> [(Note, Maybe Velocity)]
noteOutput (NoteOutput n v) = [(n, v)]
noteOutput _ = []

startRecording ::
    ('[Recording] :< state) =>
    Union (TrackStates :\ Recording) ->
    ([(Note, Maybe Velocity)], Union state)
startRecording =
       (\t@(Stopped {}) -> ([] :: [(Note, Maybe Velocity)], fromStopped $ liftUnion t))
    @> (\t@(Playing {}) -> fromStopped <$> stopPlaying t)
    @> typesExhausted
  where
    fromStopped :: ('[Recording] :< state) => Union '[Stopped] -> Union state
    fromStopped =
        (\(Stopped {})-> liftUnion $ Recording Vector.empty mempty
        ) @>
        typesExhausted

snocMany :: [a] -> Vector a -> Vector a
snocMany l v = v <> Vector.fromList l

stopRecording :: ('[Stopped] :< state) => Recording -> Union state
stopRecording r =
  liftUnion $
  Stopped
    { _trackData'Stopped = snocMany (releases $ view toRelease $ liftUnion r) $ view trackData $ liftUnion r
    }

-- | Analogous to the arrow combinator @(&&&)@.
(|&|) :: Lens' s a -> Lens' s b -> Lens' s (a, b)
l1 |&| l2 = lens view0 set0
  where
    view0 s = (view l1 s, view l2 s)
    set0 s (a, b) = set l1 a $ set l2 b $ s

recordTrack :: [TrackOutput] -> Recording -> Union '[Recording]
recordTrack outputs = liftUnion . inner
  where
    inner =
      over (trackData'Recording |&| toRelease'Recording) $
      forLoop outputs $
        \(trackData', toRelease') out ->
          let recorded = Lens.snoc trackData' out in
          case out of
            RestOutput _ ->
              if Vector.null trackData'
              then (trackData', toRelease')
              else (recorded, toRelease')
            NoteOutput n mv -> case mv of
              Nothing -> case deleteRef n toRelease' of
                Nothing -> (trackData', toRelease')
                Just rel' -> (recorded, rel')
              Just _ -> (recorded, insertRef n toRelease')

startPlaying :: ('[Playing] :< state) => LoopBehavior -> Union (TrackStates :\ Playing) -> Union state
startPlaying l =
    (\t ->
      liftUnion $
        Playing
        { _trackData'Playing = view trackData'Stopped t
        , _idx = 0
        , _leftoverT = 0
        , _toRelease'Playing = mempty
        , _loopBehavior = l
        }
    ) @>
    (\t -> startPlaying l $ stopRecording t
    ) @>
    typesExhausted

stopPlaying :: ('[Stopped] :< state) => Playing -> ([(Note, Maybe Velocity)], Union state)
stopPlaying t =
  ( Foldable.concatMap noteOutput $ releases $ view toRelease $ liftUnion t
  , liftUnion $ Stopped { _trackData'Stopped = view trackData $ liftUnion t }
  )

releases :: Refcount Note -> [TrackOutput]
releases started
    = Foldable.concatMap (\(note, n) -> BasicPrelude.replicate n $ release note)
    $ refcounts started
  where
    release note = NoteOutput note Nothing

updateNotesHeld :: Foldable t => t (Note, Maybe Velocity) -> NotesHeld -> NotesHeld
updateNotesHeld outputs t = Foldable.foldl' countNote t outputs
  where
    countNote v (n, Nothing) = fromMaybe v $ deleteRef n v
    countNote v (n, _) = insertRef n v

playSomeTrack ::
    ('[Playing, Stopped] :< state) =>
    Tick {- ^ amount of time to play -} ->
    Playing {- track state -} ->
    ([(Note, Maybe Velocity)], Union state) {- ^ (playback outputs, new state) -}
playSomeTrack = \dt t ->
    let
      l = view loopBehavior t
      (trackStateDiff, outputs) = iterateTo l (dt + view leftoverT t) (view idx t) (view trackData'Playing t) []
      t' = case trackStateDiff of
          Nothing ->
              liftUnion $
              Stopped { _trackData'Stopped = view trackData'Playing t }
          Just (idx', leftoverT') ->
              liftUnion $
              set idx idx' $
              set leftoverT leftoverT' $
              over toRelease'Playing (updateNotesHeld outputs) $
              t
    in (outputs, t')
  where
    iterateTo l t start dat outputs
        = if start < Vector.length dat
          then case dat Vector.! start of
                RestOutput dt ->
                  if dt < t
                  then iterateTo l (t - dt) (start + 1) dat outputs
                  else (Just (start, t), BasicPrelude.reverse outputs)
                NoteOutput note v -> iterateTo l t (start + 1) dat ((note, v) : outputs)
          else case l of
                Once -> (Nothing, outputs)
                -- TODO: if we try to play a track with no rests (e.g. the empty track), this asplodes. FIX.
                Loop -> iterateTo l t 0 dat outputs

track :: TrackNumber -> Lens' TrackMemory (Union TrackStates)
track t = lens (lookupDefault initialTrack t) (\m v -> HashMap.insert t v m)

initialTrack :: Union TrackStates
initialTrack =
  liftUnion $
    Stopped
      { _trackData'Stopped = Vector.empty
      }
