{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , Arrows
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import IO

import Control.Stream
import Data.Tuple

import Wrappers.GLFW
import Wrappers.Events

import Sound.MIDI.Monad

import Main.Graphics
import Main.Input

import Config
import Logic

mstream :: (Functor m, Monad m) => m a -> Stream m r a
mstream = lift . arr . \m _-> m

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        runMIDI title outMIDI inMIDI $ tempo (div 60000000 bpm)
                                    >> iterateM_ (map snd . ($< ())) mainLoop
    where
        mainLoop = inputs <&> map Just <&> (Nothing:) -- Add an update with no inputs,
                                                      -- so we're updating at least once per iteration.
               >>> map inputStep
               >>> mstream (ioMIDI $ \_-> updateGraphics >> io (sleep 0.01))

        inputStep = proc i -> do
                        deltaT <- ticks >>> identify diff -< ()
                        lift ((0,) <$$> song <&> sendNotes) -< (i, deltaT)

        sendNotes notes = traverse_ sendNote notes >> flush
        sendNote (t, (mv, n)) = (\v -> startNote drumChannel t v n) <$> mv
                            <?> stopNote drumChannel t n

        diff = loop (barr $ \t t0 -> ((t -) <$> t0 <?> 0, Just t)) Nothing

-- TODO: Investigate overflow scenarios
ticks :: Stream MIDI () Tick
ticks = mstream $ ioMIDI $ \_-> io (convert <$> getTime)
    where
        convert t = floor (t * fromIntegral bpm * 96 / 60 / fromIntegral granularity) * granularity
