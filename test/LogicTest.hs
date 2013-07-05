module LogicTest (test) where

import Prelewd

import Test

import Storage.Id
import Storage.List (take)
import Storage.Set
import Sound.MIDI.Monad.Types
import Text.Show

import Input
import Logic

newtype Inputs a = Inputs [a]

instance Arbitrary a => Arbitrary (Inputs a) where
    arbitrary = arbitrary <&> take 12 <&> Inputs

instance Show a => Show (Inputs a) where
    show (Inputs l) = show l

test :: Test
test = $(testGroupGenerator)

prop_notes :: Inputs (Velocity, Set Note) -> Result
prop_notes = streamTest song $ \(Inputs notes) -> do
                        (v, chord) <- toList <$$> notes
                        [ ioPair (Just v) chord, ioPair Nothing chord ]
    where
        -- input to expected output pairing for the stream
        ioPair v chord = ( (Just (v, Chord chord), 0)
                         , Id $ (v,) <$> chord
                         )

prop_harmony :: Inputs (Velocity, (Inputs Harmony, Inputs Note)) -> Result
prop_harmony = streamTest (set <$> song)
             $ preprocess
           >>> \inputs -> do
                    (v, (harmonies, notes)) <- inputs
                    let outNotes = set $ harmonize <$> harmonies <*> ((v,) <$> notes)
                        setNotes = (v,) <$> set notes
                    ioPair <$> [ ((Just v, Harmony harmonies), mempty)
                               , ((Just v, Chord notes), map2 Just <$> setNotes <> outNotes)
                               , ((Nothing, Harmony harmonies), unheld outNotes $ setNotes)
                               , ((Nothing, Chord notes), off <$> setNotes)
                               ]
    where
        off = map2 $ \_-> Nothing
        unheld = (\\) `on` map off

        preprocess (Inputs l) = (\(Inputs h, Inputs n) -> (toList $ set h, toList $ set n)) <$$> l
        ioPair = (Just >>> (, 0)) *** Id
