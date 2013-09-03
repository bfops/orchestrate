import Summit.IO

import Summit.Test

import qualified Logic
import qualified Data.Trie as Trie

main :: SystemIO ()
main = defaultMain
        [ Trie.test
        , Logic.test
        ]
