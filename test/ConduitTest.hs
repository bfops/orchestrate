{-# LANGUAGE TemplateHaskell #-}
module ConduitTest ( tests ) where

import Prelude ()
import BasicPrelude hiding (map, take)

import Data.Conduit.List
import Data.Functor.Identity
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Conduit.Extra

tests = $(testGroupGenerator)

case_exhaustInput
    = let
        f x = 3 * x :: Int
        t = resumable $ map f
        (ins1, ins2) = ([1, 2], [3, 4])
        Identity (next, outs1) = exhaustInput (sourceList ins1) t
        Identity (_, outs2) = exhaustInput (sourceList ins2) next
      in (outs1 ++ outs2) @?= fmap f (ins1 ++ ins2)
