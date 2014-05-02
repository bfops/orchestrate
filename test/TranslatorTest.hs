{-# LANGUAGE TemplateHaskell #-}
module TranslatorTest ( tests ) where

import Prelude ()
import BasicPrelude hiding (map)

import Data.Conduit
import Data.Conduit.List
import Data.Functor.Identity
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Translator

tests = $(testGroupGenerator)

case_simpleTranslator
    = let
        ins = [1, 5] :: [Integer]
        fs = [(+1), (*3)]
        t = translator $ map . (Produce .) <$> fs
        Identity l = sourceList ins $= t $$ consume
      in l @?= [f x | x <- ins, f <- fs]

case_oneReturns
    = let
        ins = [1, 5] :: [Integer]
        fs = [(*3)]
        t = translator $ return () : [map $ Produce . f | f <- fs]
        Identity l = sourceList ins $= t $$ consume
      in l @?= [f x | x <- ins, f <- fs]
