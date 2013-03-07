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
        sendNotes notes = traverse_ (\(b, (t, n)) -> iff b startNote stopNote t n) notes >> flush

        mainLoop = inputs
               >>> several (id &&& deltaT >>> lift (song >>> arr sendNotes))
               >>> mstream (ioMIDI $ \_-> updateGraphics >> io (sleep 0.01))

        inputs = (map Left <$$> buttons) <&> (<>) <*> (map Right <$$> mstream midiIn)
             >>> identify ( several (loop (barr convertInputs) (mapButtons, mapMIDI))
                        >>> arr (mapMaybe id)
                          )

        deltaT = identify (arr $ \_-> ()) >>> ticks >>> identify diff

        diff = loop (barr $ \t t0 -> ((t -) <$> t0 <?> 0, Just t)) Nothing

convertInputs :: (Bool, Either Button Note) -> InputMap -> (Maybe (Bool, Input), InputMap)
convertInputs (b, e) m = let i = convertInput m e
                         in ((b,) <$> i, try newMap (guard b >> i >>= fromRemap) m)
    where
        newMap (btns', notes') (btns, notes) = (btns' <> btns, notes' <> notes)

convertInput :: InputMap -> Either Button Note -> Maybe Input
convertInput (btns, _) (Left btn) = lookup btn btns
convertInput (_, notes) (Right (Note p c v)) = lookup (p, c) notes <&> ($ v) <|> Just (Melody [Note p c v])

buttons :: Stream MIDI () [(Bool, Button)]
buttons = mswitch (ioMIDI . \i _-> i)
        $ events
      >>> lift (arr $ traverse toButton)
      >>> identify (arr concat)
    where
        toButton CloseEvent = empty
        toButton (ResizeEvent s) = resize s $> []
        toButton (ButtonEvent b s) = return [(s == Press, b)]
        toButton _ = return []

-- TODO: Investigate overflow scenarios
ticks :: Stream MIDI () Tick
ticks = mstream $ ioMIDI $ \_-> io (convert <$> getTime)
    where
        convert t = floor (t * fromIntegral bpm * 96 / 60 / fromIntegral granularity) * granularity
