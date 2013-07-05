module Main (main) where

import IO

import Test

import qualified LogicTest
import qualified Trie

main :: SystemIO ()
main = defaultMain
        [ Trie.test
        , LogicTest.test
        ]
