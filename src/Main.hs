{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , Arrows
           #-}
-- | Main module, entry point
module Main (main) where

import Summit.Control.Stream
import Summit.Data.Map
import Summit.IO
import Summit.Prelewd

import Control.Stream.Util
import Data.Tuple

import Wrappers.GLFW
import Wrappers.Events

import Sound.MIDI.Monad

import Main.Graphics
import Main.Input

import Logic

windowSize :: Num a => (a, a)
windowSize = (64, 64)

-- | GLFW display options
displayOpts :: DisplayOptions
displayOpts = defaultDisplayOptions
    { displayOptions_width = fst windowSize
    , displayOptions_height = snd windowSize
    , displayOptions_windowIsResizable = True
    }

-- | Title of the game window
title :: Text
title = "Soundflow"

-- | Output MIDI destinations
outMIDI :: [Text]
outMIDI = ["128:0"]

-- | Map MIDI sources to instruments
inMIDI :: Map Text Instrument
inMIDI = fromList []

-- | Beats per minute
bpm :: Integer
bpm = 60

granularity :: Tick
granularity = 2

midiStream :: IO a -> Stream MIDI r a
midiStream m = lift $ arr $ \_-> ioMIDI $ \_-> m

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
               >>> map (id &&& deltaT >>> logic)
               >>> midiStream ioUpdates

        ioUpdates = updateGraphics
                 >> io (sleep 0.01)

deltaT :: Stream MIDI a Tick
deltaT = ticks >>> identify delta
  where
    delta = proc t -> do
              tPrev <- previous Nothing -< Just t
              id -< (t -) <$> tPrev <?> 0

    -- TODO: Investigate overflow scenarios
    ticks :: Stream MIDI a Tick
    ticks = midiStream $ io $ convert <$> getTime
        where
            convert t = floor (t * fromIntegral bpm * 96 / 60 / fromIntegral granularity)
                      * granularity
