import Summit.IO

import Summit.Test

import qualified Data.Trie as Trie
import qualified Logic.Sequential as Seq

main :: SystemIO ()
main = defaultMain
        [ Trie.test
        , Seq.test
        ]
