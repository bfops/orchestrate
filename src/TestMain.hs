import Summit.IO

import Summit.Test

import qualified Data.Trie as Trie
import qualified Logic.Sequential as Seq
import qualified Logic.Memory as Memory

main :: SystemIO ()
main = defaultMain
        [ Trie.test
        , Seq.test
        , Memory.test
        ]
