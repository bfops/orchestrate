{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import IO

import Control.Stream
import Data.Tuple
import Storage.Map (lookup)

import Wrappers.GLFW
import Wrappers.Events

import Sound.MIDI.Monad

import Main.Graphics

import Config

-- | Loop with an iterator.. forever
loop :: Monad m => (a -> m a) -> a -> m a
loop f x = f x >>= loop f

mswitch :: Functor m2 => (m1 (a, Stream m1 r a) -> m2 (b, Stream m1 r a)) -> Stream m1 r a -> Stream m2 r b
mswitch f s = Stream $ \r -> mswitch f <$$> f (s $< r)

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        runMIDI title "128:0" "14:0" $ void $ loop mainLoop
                                     $ (midiEvents <&> (<>) <*> lift (arr $ \_-> midiIn))
                                   >>> several (lift $ arr sendNote)
                                   >>> lift (arr $ \_-> ioMIDI $ \_-> updateGraphics)
    where
        sendNote (b, note) = iff b startNote stopNote 0 note >> flush

mainLoop :: Stream MIDI () () -> MIDI (Stream MIDI () ())
mainLoop s = (snd <$> s $< ()) <* ioMIDI (\_-> io $ sleep 0.01)

midiEvents :: Stream MIDI () [(Bool, Note)]
midiEvents = mswitch (ioMIDI . \i _-> i)
           $ events >>> lift (arr $ traverse convertEvent) >>> identify (arr $ mapMaybe id)
    where
        convertEvent CloseEvent = empty
        convertEvent (ResizeEvent s) = resize s $> Nothing
        convertEvent (ButtonEvent (KeyButton key) s) = return $ (s == Press,) <$> lookup key keymap
        convertEvent _ = return Nothing
