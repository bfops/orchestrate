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
granularity = 6

-- | What controls what?
mapButtons :: ButtonMap
mapButtons = fromList $ noteButtons <> harmonyButtons <> recordButtons <> remapButtons
    where
        makeNote p = Note p 0 64

        noteButtons = map (KeyButton . CharKey *** Melody . map makeNote)
            [ ('A', [48])
            , ('S', [50])
            , ('D', [52])
            , ('F', [53])
            , ('G', [55])
            , ('H', [57])
            , ('J', [59])
            , ('K', [60])
            ]

        harmonyButtons = map (KeyButton . CharKey *** Harmony)
            [("0123456789" ! i, [fromInteger i]) | i <- [1..9]]

        recordButtons = map (map2 $ KeyButton . CharKey)
            [('Q', Record), ('W', Play)]

        remapButtons = map (KeyButton . CharKey *** Remap . map fromList)
            [ ('R', (mempty, violinMaps))
            ]

        violinMaps = map (Melody <$$>)
            [((36 + i, 0), (: []) . Note (48 + i) 40) | i <- [0..23]]

mapMIDI :: MIDIMap
mapMIDI = fromList mempty
