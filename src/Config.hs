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
import Storage.Trie

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
mapInput = fromMap (mapKeys (map Left) $ fromList $ harmonyButtons <> recordButtons) <> pianoMap
    where
        harmonyButtons = map ((:[]) . KeyButton . CharKey *** Harmony)
            [(numChar i, [(Nothing, fromInteger i)]) | i <- [1..9]]

        recordButtons = map (map (KeyButton . CharKey) *** id)
                      $ do i <- [1..9]
                           let c = numChar i
                           [(['Z', c], Record i), (['X', c], Play i)]

piano :: Pitch -> Note
piano = (, Instrument 0)

pianoMIDI :: InputMap
pianoMIDI = fromMap $ fromList $ ((:[]) . Right *** Chord) <$> [(piano n, [piano n]) | n <- [0..120]]

pianoMap :: InputMap
pianoMap = fromMap (fromList $ noteButtons <> harmonyButtons <> remapButtons) <> pianoMIDI
    where
        noteButtons = map ((:[]) . Left . KeyButton . CharKey *** Chord . map piano)
            [("ASDFGHJK" ! i, [[48, 50, 52, 53, 55, 57, 59, 60] ! i]) | i <- [0..7]]

        harmonyButtons = map ((:[]) . Left . KeyButton . CharKey *** Harmony)
            [("QWERTYUIOP" ! i, [(Just $ Instrument 40, fromInteger i)]) | i <- [0..9]]

        remapButtons = map ((:[]) . Left . KeyButton . CharKey *** Remap)
            [ (';', violinMap)
            ]

violinMap :: InputMap
violinMap = fromMap
          $ mapKeys (map Left) (fromList $ noteButtons <> harmonyButtons <> remapButtons)
         <> mapKeys (map Right) (fromList violinMIDI)
    where
        violin = (, Instrument 40)

        noteButtons = map ((:[]) . KeyButton . CharKey *** Chord . map violin)
            [("ASDFGHJK" ! i, [[48, 50, 52, 53, 55, 57, 59, 60] ! i]) | i <- [0..7]]

        harmonyButtons = map ((:[]) . KeyButton . CharKey *** Harmony)
            [("QWERTYUIOP" ! i, [(Just $ Instrument 0, fromInteger i)]) | i <- [0..9]]

        remapButtons = map ((:[]) . KeyButton . CharKey *** Remap)
            [ (';', pianoMap)
            ]

        violinMIDI = map ((:[]) *** Chord)
            [((36 + i, Instrument 0), [(48 + i, Instrument 40)]) | i <- [0..23]]
