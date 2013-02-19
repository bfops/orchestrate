{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import IO

import Control.Stream
import Data.Int
import Data.Tuple
import Storage.Id
import Storage.Map
import Storage.Set (Set, set)
import Subset.Num

import Wrappers.GLFW
import Wrappers.Events

import Sound.MIDI.Monad

import Main.Graphics

import Config

import Input

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
                                     $ (inputs <&> (<>) <*> midiInputs)
                                   >>> several (lift $ sendNote <$> harmonies <*> arr (map fromNote))
                                   >>> lift (arr $ \_-> ioMIDI $ \_-> updateGraphics)
    where
        sendNote h (b, Just note) = traverse_ (iff b startNote stopNote 0 . adjustPitch note) h >> flush
        sendNote _ _ = return ()

        adjustPitch note dp = pitch' (fromIntegral . (dp +) . fromIntegral) note

harmonies :: Stream Id (Bool, Input) (Set Int16)
harmonies = arr Just >>> updater (Id <$$> newInputMap . map fromHarmony) initHarmonies >>> arr (set . keys)
    where
        initHarmonies = Id $ singleton 0 (1 :: Positive Integer)

        newInputMap (_, Nothing) m = m
        newInputMap (True, Just shift) m = insertWith (+) shift 1 m
        newInputMap (False, Just shift) m = modify (\v -> toPos $ fromPos v - 1) shift m <?> m

mainLoop :: Stream MIDI () () -> MIDI (Stream MIDI () ())
mainLoop s = (snd <$> s $< ()) <* ioMIDI (\_-> io $ sleep 0.01)

midiInputs :: Stream MIDI () [(Bool, Input)]
midiInputs = lift $ arr $ \_-> map NoteKey <$$> midiIn

inputs :: Stream MIDI () [(Bool, Input)]
inputs = mswitch (ioMIDI . \i _-> i)
       $ events >>> lift (arr $ traverse convertEvent) >>> identify (arr $ mapMaybe id)
    where
        convertEvent CloseEvent = empty
        convertEvent (ResizeEvent s) = resize s $> Nothing
        convertEvent (ButtonEvent (KeyButton key) s) = return $ (s == Press,) <$> lookup key keymap
        convertEvent _ = return Nothing
