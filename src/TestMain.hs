import IO

import Test

import qualified Logic
import qualified Storage.Trie as Trie

main :: SystemIO ()
main = defaultMain
        [ Trie.test
        , Logic.test
        ]
