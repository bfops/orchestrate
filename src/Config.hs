{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Settings are stored in this module
module Config ( windowSize
              , displayOpts
              , title
              , inMIDI
              , outMIDI
              , bpm
              , granularity
              , defaultVelocity
              , mapInput
              ) where

import Prelewd

import Data.Char
import Data.Tuple
import Storage.Map

import Sound.MIDI.Monad.Types

import Wrappers.Events
import Wrappers.GLFW (DisplayOptions (..), defaultDisplayOptions)

import Input

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

outMIDI, inMIDI :: Text
outMIDI = "128:0"
inMIDI = "14:0"

-- | Beats per minute
bpm :: Integer
bpm = 60

granularity :: Tick
granularity = 2

numChar :: Integer -> Char
numChar i = "0123456789" ! i

defaultVelocity :: Velocity
defaultVelocity = 64

-- | What controls what?
mapInput :: InputMap
mapInput = mapKeys Left (fromList $ harmonyButtons <> recordButtons) <> pianoMap
    where
        harmonyButtons = map (KeyButton . CharKey *** (\h _-> h) . Harmony)
            [(numChar i, [(Nothing, fromInteger i)]) | i <- [1..9]]

        recordButtons = map (KeyButton . CharKey *** \i _-> i)
            [('Z', Record), ('X', Play)]

pianoMap :: InputMap
pianoMap = mapKeys Left $ fromList $ noteButtons <> harmonyButtons <> remapButtons
    where
        makePiano v p = Note p (Instrument 0) v

        noteButtons = map (KeyButton . CharKey *** \p v -> Melody $ map (makePiano v) p)
            [("ASDFGHJK" ! i, [[48, 50, 52, 53, 55, 57, 59, 60] ! i]) | i <- [0..7]]

        harmonyButtons = map (KeyButton . CharKey *** (\h _-> h) . Harmony)
            [("QWERTYUIOP" ! i, [(Just $ Instrument 40, fromInteger i)]) | i <- [0..9]]

        remapButtons = map (KeyButton . CharKey *** (\r _-> r) . Remap)
            [ ('V', violinMap)
            ]

violinMap :: InputMap
violinMap = mapKeys Left (fromList $ noteButtons <> harmonyButtons <> remapButtons)
         <> mapKeys Right (fromList violinMIDI)
    where
        makeViolin v p = Note p (Instrument 40) v

        noteButtons = map (KeyButton . CharKey *** \p v -> Melody $ map (makeViolin v) p)
            [("ASDFGHJK" ! i, [[48, 50, 52, 53, 55, 57, 59, 60] ! i]) | i <- [0..7]]

        harmonyButtons = map (KeyButton . CharKey *** (\h _-> h) . Harmony)
            [("QWERTYUIOP" ! i, [(Just $ Instrument 0, fromInteger i)]) | i <- [0..9]]

        remapButtons = map (KeyButton . CharKey *** (\r _-> r) . Remap)
            [ ('V', (pianoMap <> mapKeys Right (fromList pianoMIDI)))
            ]

        violinMIDI = map (Melody <$$>)
            [((36 + i, Instrument 0), (:[]) . Note (48 + i) (Instrument 40)) | i <- [0..23]]

        pianoMIDI = map (Melody <$$>)
            [((36 + i, Instrument 0), (:[]) . Note (36 + i) (Instrument 0)) | i <- [0..23]]
