module Translator.Keyboard ( matchKeyEvent
                           , matchKeyPress
                           , matchKeyPressAndRelease
                           , matchCharKey
                           , keyPress
                           , charKey
                           , numKey
                           ) where

import Prelude ()
import BasicPrelude

import Data.Conduit
import Data.Conduit.Extra

import Wrappers.Events

import Input
import Translator

matchKeyEvent ::
    Monad m =>
    Key ->
    KeyState ->
    (UnifiedEvent -> Translator m UnifiedEvent o Bool) ->
    Translator m UnifiedEvent o Bool
matchKeyEvent key state = match $ keyPress key state

keyPress :: Key -> KeyState -> UnifiedEvent -> Bool
keyPress key state (Event (KeyEvent k s _)) = k == key && s == state
keyPress _ _ _ = False

matchKeyPress ::
    Monad m =>
    Key ->
    Translator m UnifiedEvent o Bool ->
    Translator m UnifiedEvent o Bool
matchKeyPress key t = matchKeyEvent key KeyState'Pressed $ \_-> t

matchKeyPressAndRelease ::
    Monad m =>
    Key ->
    (UnifiedEvent -> Translator m UnifiedEvent o Bool) ->
    Translator m UnifiedEvent o Bool
matchKeyPressAndRelease key continue
    = matchKeyEvent key KeyState'Pressed
    $ \e -> do
          yield $ AddTranslator (untilTrue $ matchKeyEvent key KeyState'Released continue)
          continue e

matchCharKey ::
    Monad m =>
    Char ->
    (UnifiedEvent -> Translator m UnifiedEvent o Bool) ->
    Translator m UnifiedEvent o Bool
matchCharKey = matchKeyPressAndRelease . charKey

numKey :: Integral i => i -> Key
numKey i = [Key'0, Key'1, Key'2, Key'3, Key'4, Key'5, Key'6, Key'7, Key'8, Key'9] !! fromIntegral i

charKey :: Char -> Key
charKey c = toEnum $ fromEnum Key'A + ((-) `on` fromEnum) c 'A'
