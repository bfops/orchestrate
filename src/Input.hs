{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
-- | Program-specific input types
module Input ( Input (..)
             , TrackCommand (..)
             , UnifiedEvent (..)
             , PitchShift (..)
             , Harmony (..)
             , EventTranslator
             , EventTranslation
             , Track
             , emptyHarmony
             , velocity
             , harmonize
             ) where

import Prelude ()
import BasicPrelude

import Control.Lens
import Data.Text

import Sound.MIDI.Types

import Wrappers.Events

import Translator

-- | Cap a number between its min and max.
bound :: (Ord a, Bounded a) => a -> a
bound = min maxBound . max minBound

data UnifiedEvent
      = Event Event
      | Midi Note (Maybe Velocity)
  deriving (Show, Eq, Ord)

data PitchShift
    = DeltaPitch Pitch
    | Absolute Pitch
  deriving (Show, Read, Eq, Ord)

instance Hashable PitchShift where
  hashWithSalt s p = hashWithSalt s $ case p of
                        Absolute a -> Left a
                        DeltaPitch dp -> Right dp

data Harmony = Harmony
    { changePitch :: Maybe PitchShift
    , changeInstrument :: Maybe Instrument
    , changeVelocity :: Maybe Velocity
    }
  deriving (Show, Read, Eq, Ord)

instance Hashable Harmony where
  hashWithSalt s (Harmony a b c) = hashWithSalt s (a, b, c)

emptyHarmony :: Harmony 
emptyHarmony = Harmony Nothing Nothing Nothing

type EventTranslation m = Translation m UnifiedEvent Input
type EventTranslator m = Translator m UnifiedEvent Input

type Track = Integer

data Input
        = NoteInput Note (Maybe Velocity)
        | HarmonyInput Harmony Bool
        | Track TrackCommand Track
        | Timestep Tick
    deriving (Show, Read, Eq, Ord)

data TrackCommand = Record
                  | Play
                  | Save
                  | Load
    deriving (Show, Read, Eq, Ord)

-- | @Nothing@ for an off signal, @Just v@ for an on signal with velocity.
velocity ::
    Velocity {-^ default velocity for on events with no velocity details -} ->
    Getter UnifiedEvent (Maybe Velocity)
velocity def = to getVelocity
  where
    getVelocity (Event (KeyEvent _ KeyState'Pressed _)) = Just def
    getVelocity (Event (KeyEvent _ KeyState'Released _)) = Nothing
    getVelocity (Midi _ v) = Just $ fromMaybe def v
    getVelocity e = error $ "velocity not defined for event " <> unpack (show e)

-- | Apply a harmony to a note.
harmonize :: Harmony -> (Note, Maybe Velocity) -> (Note, Maybe Velocity)
harmonize h ((p, i), v)
    = let v' = fromIntegral . bound . maybe id (+) (changeVelocity h) . fromIntegral <$> v
          p' = case changePitch h of
                  Nothing -> p
                  Just (Absolute a) -> a
                  Just (DeltaPitch dp) -> fromIntegral $ bound $ fromIntegral p + dp
          inst' = fromMaybe i $ changeInstrument h
      in ((p', inst'), v')
