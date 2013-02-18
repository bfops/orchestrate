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
keymap :: Map Key Note
keymap = map makeNote . mapKeys CharKey $ fromList
       [ ('A', 48)
       , ('S', 50)
       , ('D', 52)
       , ('F', 53)
       , ('G', 55)
       , ('H', 57)
       , ('J', 59)
       , ('K', 60)
       ]
    where
        makeNote p = Note p 0 64
