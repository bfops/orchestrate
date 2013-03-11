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
              , mapButtons
              , mapMIDI
              ) where

import Prelewd

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

-- | What controls what?
mapButtons :: ButtonMap
mapButtons = fromList (harmonyButtons <> recordButtons) <> pianoButtons
    where
        harmonyButtons = map (KeyButton . CharKey *** Harmony)
            [("_123456789" ! i, [(Nothing, fromInteger i)]) | i <- [1..9]]

        recordButtons = map (map2 $ KeyButton . CharKey)
            [('Z', Record), ('X', Play)]

pianoButtons :: ButtonMap
pianoButtons = fromList $ noteButtons <> harmonyButtons <> remapButtons
    where
        makeNote p = Note p (Instrument 0) 64

        noteButtons = map (KeyButton . CharKey *** Melody . map makeNote)
            [("ASDFGHJK" ! i, [[48, 50, 52, 53, 55, 57, 59, 60] ! i]) | i <- [0..7]]

        harmonyButtons = map (KeyButton . CharKey *** Harmony)
            [("QWERTYUIOP" ! i, [(Just $ Instrument 40, fromInteger i)]) | i <- [0..9]]

        remapButtons = map (KeyButton . CharKey *** Remap)
            [ ('V', violinMap)
            ]

violinMap :: InputMap
violinMap = (fromList $ noteButtons <> harmonyButtons <> remapButtons, fromList violinMIDI)
    where
        makeNote p = Note p (Instrument 40) 64

        noteButtons = map (KeyButton . CharKey *** Melody . map makeNote)
            [("ASDFGHJK" ! i, [[48, 50, 52, 53, 55, 57, 59, 60] ! i]) | i <- [0..7]]

        harmonyButtons = map (KeyButton . CharKey *** Harmony)
            [("QWERTYUIOP" ! i, [(Just $ Instrument 0, fromInteger i)]) | i <- [0..9]]

        remapButtons = map (KeyButton . CharKey *** Remap)
            [ ('V', (pianoButtons, fromList pianoMIDI))
            ]

        violinMIDI = map (Melody <$$>)
            [((36 + i, Instrument 0), (: []) . Note (48 + i) (Instrument 40)) | i <- [0..23]]

        pianoMIDI = map (Melody <$$>)
            [((36 + i, Instrument 0), (: []) . Note (36 + i) (Instrument 0)) | i <- [0..23]]

mapMIDI :: MIDIMap
mapMIDI = fromList mempty
