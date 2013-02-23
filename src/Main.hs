{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import IO

import Control.Stream
import Data.Tuple
import Storage.Map

import Wrappers.GLFW
import Wrappers.Events

import Sound.MIDI.Monad

import Main.Graphics

import Config

import Input
import Logic

mswitch :: Functor m2 => (m1 (a, Stream m1 r a) -> m2 (b, Stream m1 r a)) -> Stream m1 r a -> Stream m2 r b
mswitch f s = Stream $ \r -> mswitch f <$$> f (s $< r)

mstream :: (Functor m, Monad m) => m a -> Stream m () a
mstream = lift . arr . \m _-> m

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        runMIDI title "128:0" "14:0" $ tempo (div 60000000 bpm)
                                    >> iterateM_ (map snd . ($< ())) mainLoop
    where
        sendNotes notes = traverse_ (\(b, (t, n)) -> iff b startNote stopNote t n) notes >> flush

        diffT = loop (barr $ \((b, inpt), t) t0 -> (((b, inpt), (t -) <$> t0 <?> 0), Just t)) Nothing

        mainLoop = (inputs <&> (<>) <*> midiInputs) &&& ticks
               >>> arr (\(l, t) -> l <&> (, t))
               >>> several (identify diffT >>> lift (song >>> arr sendNotes))
               >>> mstream (ioMIDI $ \_-> updateGraphics >> io (sleep 0.01))

inputs :: Stream MIDI () [(Bool, Input)]
inputs = mswitch (ioMIDI . \i _-> i)
       $ events >>> lift (arr $ traverse convertEvent) >>> identify (arr $ mapMaybe id)
    where
        convertEvent CloseEvent = empty
        convertEvent (ResizeEvent s) = resize s $> Nothing
        convertEvent (ButtonEvent (KeyButton key) s) = return $ (s == Press,) <$> lookup key keymap
        convertEvent _ = return Nothing

midiInputs :: Stream MIDI () [(Bool, Input)]
midiInputs = lift $ arr $ \_-> map toInput <$$> midiIn
    where
        toInput (Note p c v) = lookup (p, c) midiMap <&> ($ v) <?> Melody [Note p c v]

-- TODO: Investigate overflow scenarios
ticks :: Stream MIDI () Tick
ticks = mstream $ ioMIDI $ \_-> io (convert <$> getTime)
    where
        convert t = floor (t * fromIntegral bpm * 96 / 60 / fromIntegral granularity) * granularity
