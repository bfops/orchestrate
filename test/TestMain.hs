import Test.Framework

import ConduitTest
import LogicTest
import TranslatorTest
import TranslatorTest.Keyboard

main :: IO ()
main = defaultMain
    [ TranslatorTest.tests
    , TranslatorTest.Keyboard.tests
    , ConduitTest.tests
    , LogicTest.tests
    ]
