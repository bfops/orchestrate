{-# LANGUAGE TemplateHaskell #-}
module LogicTest (tests) where

import Control.Eff.Lift
import Data.Conduit
import Data.Conduit.List
import Sound.MIDI.Types
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Input
import Logic

tests = $(testGroupGenerator)

case_OnOffNote
    = let
        ins = 
          [ NoteInput (Pitch 48, read "AcousticGrandPiano") (Just (Velocity 64))
          , NoteInput (Pitch 52, read "AcousticGrandPiano") (Just (Velocity 64))
          , NoteInput (Pitch 52, read "AcousticGrandPiano") Nothing
          , NoteInput (Pitch 55, read "AcousticGrandPiano") (Just (Velocity 64))
          ]
      in do
          l <- runLift $ sourceList ins $= logic $$ consume
          l @?= fmap (\(NoteInput (p, i) v) -> ((p, i), v)) ins
