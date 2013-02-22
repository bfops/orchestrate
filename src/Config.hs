{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Settings are stored in this module
module Config ( windowSize
              , displayOpts
              , title
              , bpm
              , granularity
              , keymap
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

-- | Beats per minute
bpm :: Integer
bpm = 60

granularity :: Tick
granularity = 6

-- | What controls what?
keymap :: Map Key Input
keymap = fromList $ map (CharKey *** Melody . map makeNote) noteKeys
                 <> map (CharKey *** Harmony) harmonyKeys
                 <> [(CharKey 'Q', Record), (CharKey 'W', Play)]
    where
        makeNote p = Note p 0 64

        noteKeys =
            [ ('A', [48])
            , ('S', [50])
            , ('D', [52])
            , ('F', [53])
            , ('G', [55])
            , ('H', [57])
            , ('J', [59])
            , ('K', [60])
            ]

        harmonyKeys = [("0123456789" ! i, [fromInteger i]) | i <- [1..9]]
