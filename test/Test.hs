module Main (main) where

import IO

import Test.Framework

import Trie

main :: SystemIO ()
main = defaultMain
        [ Trie.test
        ]
