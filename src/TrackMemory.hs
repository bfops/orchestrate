{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module TrackMemory ( TrackMemory (..)
                   , TrackOutput (..)
                   , MemoryBank
                   , trackData
                   , recording
                   , playState
                   , isPlaying
                   , track
                   ) where

import Prelude ()
import BasicPrelude hiding (insert)

import Control.Lens
import Data.HashMap.Strict
import Data.Text
import Sound.MIDI.Types

import Input

data TrackOutput
    = RestOutput Tick
    | NoteOutput Note (Maybe Velocity)
  deriving (Show, Read)

data TrackMemory = TrackMemory
        { _trackData :: Vector TrackOutput
        , _recording :: Bool
        , _playState :: Maybe (Int, Tick, LoopBehavior)
        }
  deriving (Show, Read)

$(makeLenses ''TrackMemory)

type MemoryBank = HashMap Track TrackMemory

isPlaying :: IndexPreservingGetter TrackMemory Bool
isPlaying = to $ isJust . view playState

track :: Track -> Lens' MemoryBank TrackMemory
track t = lens (lookupDefault trackNotFoundError t) (\m v -> insert t v m)
  where
    trackNotFoundError = error $ "Could not find track " <> unpack (show t)
