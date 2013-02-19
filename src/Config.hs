{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Settings are stored in this module
module Config ( viewDist
              , windowSize
              , displayOpts
              , title
              , keymap
              ) where

import Prelewd

import Data.Tuple
import Storage.Map

import Sound.MIDI.Monad.Types

import Wrappers.Events
import Wrappers.GLFW (DisplayOptions (..), defaultDisplayOptions)

import Input

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 3

windowSize :: Num a => (a, a)
windowSize = (800, 800)

-- | GLFW display options
displayOpts :: DisplayOptions
displayOpts = defaultDisplayOptions
    { displayOptions_width = fst windowSize
    , displayOptions_height = snd windowSize
    , displayOptions_windowIsResizable = False
    }

-- | Title of the game window
title :: Text
title = "Soundflow"

-- | What controls what?
keymap :: Map Key Input
keymap = fromList $ map (CharKey *** NoteKey . makeNote) noteKeys
                 <> map (CharKey *** Harmony) harmonyKeys
    where
        makeNote p = Note p 0 64

        noteKeys =
            [ ('A', 48)
            , ('S', 50)
            , ('D', 52)
            , ('F', 53)
            , ('G', 55)
            , ('H', 57)
            , ('J', 59)
            , ('K', 60)
            ]

        harmonyKeys = [("0123456789" ! i, fromInteger i) | i <- [1..9]]
