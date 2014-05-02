{-# LANGUAGE TemplateHaskell #-}
module TranslatorTest.Keyboard ( tests ) where

import Prelude ()
import BasicPrelude

import Data.Conduit
import Data.Conduit.List (sourceList, consume)
import Data.Functor.Identity
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Wrappers.Events

import Input
import Translator
import Translator.Keyboard

tests = $(testGroupGenerator)

case_simpleKeyboardTranslator
    = let
        (ins, outs) = unzip
          [ (Event (KeyEvent (charKey 'A') KeyState'Pressed noModifiers), Just ())
          , (Event (KeyEvent (charKey 'Z') KeyState'Pressed noModifiers), Nothing)
          , (Event (KeyEvent (charKey 'A') KeyState'Repeating noModifiers), Nothing)
          , (Event (KeyEvent (charKey 'A') KeyState'Released noModifiers), Just ())
          , (Event (KeyEvent (charKey 'Z') KeyState'Released noModifiers), Nothing)
          ]
        t = translator [forever $ matchCharKey 'A' $ \_-> True <$ yield (Produce ())]
        Identity l = sourceList ins $= t $$ consume
      in l @?= catMaybes outs
