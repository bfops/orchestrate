{-# LANGUAGE NoImplicitPrelude
           , TupleSections
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
        sendNotes notes = traverse_ (\(t, (mv, n)) -> (\v -> startNote t v n) <$> mv <?> stopNote t n) notes >> flush

        mainLoop = inputs
               >>> arr (map Just >>> (Nothing :))
               >>> several (id &&& deltaT >>> lift (song >>> arr (map (0, )) >>> arr sendNotes))
               >>> mstream (ioMIDI $ \_-> updateGraphics >> io (sleep 0.01))

        deltaT = identify (arr $ \_-> ()) >>> ticks >>> identify diff

        diff = loop (barr $ \t t0 -> ((t -) <$> t0 <?> 0, Just t)) Nothing

-- TODO: Investigate overflow scenarios
ticks :: Stream MIDI () Tick
ticks = mstream $ ioMIDI $ \_-> io (convert <$> getTime)
    where
        convert t = floor (t * fromIntegral bpm * 96 / 60 / fromIntegral granularity) * granularity
