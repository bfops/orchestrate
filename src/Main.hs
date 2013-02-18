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
        runMIDI title $ void $ loop mainLoop
                             $ midiEvents
                           >>> several (convertEvents >>> lift (arr $ maybe (return ()) sendNote))
                           >>> lift (arr $ \_-> ioMIDI $ \_-> updateGraphics)
    where
        sendNote (b, note) = iff b startNote stopNote 0 note >> flush

mainLoop :: Stream MIDI () () -> MIDI (Stream MIDI () ())
mainLoop s = (snd <$> s $< ()) <* ioMIDI (\_-> io $ sleep 0.01)

midiEvents :: Stream MIDI () [Event]
midiEvents = mswitch (ioMIDI . \i _-> i) events

convertEvents :: Stream MIDI Event (Maybe (Bool, Note))
convertEvents = lift $ arr $ \e -> case e of
                    CloseEvent -> empty
                    ResizeEvent s -> ioMIDI $ \_-> resize s $> Nothing
                    ButtonEvent (KeyButton key) s -> return $ (s == Press,) <$> lookup key keymap
                    _ -> return Nothing
