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
import Data.Vector as Vector
import Sound.MIDI.Types

import Input

data TrackOutput
    = RestOutput Tick
    | NoteOutput Note (Maybe Velocity)
  deriving (Show, Read)

data Stopped
    = Stopped
      { _trackData'Stopped :: Vector TrackOutput
      }
  deriving (Show, Typeable)

data Recording
    = Recording
      { _trackData'Recording :: Vector TrackOutput
      }
  deriving (Show, Typeable)

data Playing
    = Playing
      { _trackData'Playing :: Vector TrackOutput
      , _idx :: Int -- ^ the index of the play head in `trackData`
      , _leftoverT :: Tick -- ^ time is quantized via `TrackOutput`s; this is the amount of unresolved "leftover" time that wasn't prop
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

startRecording ::
    ('[Recording] :< state) =>
    Union (TrackStates :\ Recording) ->
    Union state
startRecording =
       (\t@(Stopped {}) -> fromStopped $ liftUnion t)
    @> (\t@(Playing {}) -> startRecording $ stopPlaying t)
    @> typesExhausted
  where
    fromStopped :: ('[Recording] :< state) => Union '[Stopped] -> Union state
    fromStopped =
        (\(Stopped {})-> liftUnion $ Recording Vector.empty
        ) @>
        typesExhausted

stopRecording :: ('[Stopped] :< state) => Recording -> Union state
stopRecording r =
  liftUnion $
  Stopped
    { _trackData'Stopped = view trackData $ liftUnion r
    }

-- | `foldl'` with a different argument order.
forLoop :: Foldable t => t a -> (b -> a -> b) -> b -> b
forLoop t f b = Foldable.foldl' f b t
{-# INLINE forLoop #-}

recordTrack :: [TrackOutput] -> Recording -> Union '[Recording]
recordTrack outputs = liftUnion . inner
  where
    inner =
      over trackData'Recording $
      forLoop outputs $
        \trackData' out ->
          let recorded = Lens.snoc trackData' out in
          case out of
            RestOutput _ ->
              if Vector.null trackData'
              then trackData'
              else recorded
            _ -> recorded

record :: [TrackOutput] -> TrackMemory -> TrackMemory
record outputs = fmap $ reUnion . recordTrack outputs @> reUnion

startPlaying :: ('[Playing] :< state) => LoopBehavior -> Union (TrackStates :\ Playing) -> Union state
startPlaying l =
    (\t ->
      liftUnion $
        Playing
        { _trackData'Playing = view trackData'Stopped t
        , _idx = 0
        , _leftoverT = 0
        , _loopBehavior = l
        }
    ) @>
    (\t -> startPlaying l $ stopRecording t
    ) @>
    typesExhausted

stopPlaying :: ('[Stopped] :< state) => Playing -> Union state
stopPlaying t =
  liftUnion $
  Stopped
    { _trackData'Stopped = view trackData $ liftUnion t
    }

playSome :: Tick -> TrackMemory -> ([(Note, Maybe Velocity)], TrackMemory)
playSome dt mem =
    let (outputs, mem') =
          BasicPrelude.unzip $
          fmap (\(t, (o, d)) -> (o, (t, d))) $
          HashMap.toList $
          (playSomeTrack dt @> ([],) . reUnion) <$> mem
    in (Foldable.concat outputs, HashMap.fromList mem')

playSomeTrack ::
    ('[Playing, Stopped] :< state) =>
    Tick {- ^ amount of time to play -} ->
    Playing {- track state -} ->
    ([(Note, Maybe Velocity)], Union state) {- ^ (playback outputs, new state) -}
playSomeTrack = \dt t ->
    let
      l = view loopBehavior t
      (trackStateDiff, outputs) = continueTo l (dt + view leftoverT t) (view idx t) (view trackData'Playing t) []
      t' = case trackStateDiff of
          Nothing ->
              liftUnion $
              Stopped { _trackData'Stopped = view trackData'Playing t }
          Just (idx', leftoverT') ->
              liftUnion $
              set idx idx' $
              set leftoverT leftoverT' $
              t
    in (outputs, t')
  where
    continueTo l t start dat outputs
        = if start < Vector.length dat
          then case dat Vector.! start of
                RestOutput dt ->
                  if dt < t
                  then continueTo l (t - dt) (start + 1) dat outputs
                  else (Just (start, t), BasicPrelude.reverse outputs)
                NoteOutput note v -> continueTo l t (start + 1) dat ((note, v) : outputs)
          else case l of
                Once -> (Nothing, outputs)
                -- TODO: if we try to play a track with no rests (e.g. the empty track), this asplodes. FIX.
                Loop -> continueTo l t 0 dat outputs

track :: TrackNumber -> Lens' TrackMemory (Union TrackStates)
track t = lens (lookupDefault initialTrack t) (\m v -> HashMap.insert t v m)

initialTrack :: Union TrackStates
initialTrack =
  liftUnion $
    Stopped
      { _trackData'Stopped = Vector.empty
      }
