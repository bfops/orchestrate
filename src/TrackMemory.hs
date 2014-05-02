{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module TrackMemory ( TrackMemory (..)
                   , MemoryBank
                   , track
                   , trackData
                   , recording
                   , playState
                   ) where

import Prelude ()
import BasicPrelude hiding (insert)

import Control.Lens
import Data.HashMap.Strict
import Data.Text
import Sound.MIDI.Types

import Input

data TrackMemory = TrackMemory
        { _trackData :: Vector (Either Tick (Note, Maybe Velocity)) -- ^ Either a timestep or an output
        , _recording :: Bool
        , _playState :: Maybe (Int, Tick)
        }
  deriving (Show)

$(makeLenses ''TrackMemory)

type MemoryBank = HashMap Track TrackMemory

track :: Track -> Lens' MemoryBank TrackMemory
track t = lens (lookupDefault trackNotFoundError t) (\m v -> insert t v m)
  where
    trackNotFoundError = error $ "Could not find track " <> unpack (show t)
