{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , Arrows
           , TemplateHaskell
           #-}
-- | Monad-free input-to-note translation logic with no externally accessible
-- state.
module Logic.Sequential ( seqLogic
                        , Logic.Sequential.test
                        ) where

import Summit.Control.Stream
import Summit.Data.Id
import Summit.Data.List (take)
import Summit.Data.Set (Set, set, insert)
import Summit.Prelewd
import Summit.Test

import Control.Stream.Util
import Data.Multimap
import Data.Tuple

import Sound.MIDI.Types

import Input
import Types

seqLogic :: Stream Id (Maybe (Maybe Velocity, Input)) Chord
seqLogic = bind (barr $ \v i -> (v,) <$$> (Left <$$> fromChord i <|> Right <$$> fromHarmony i))
       >>> map (concatMap inputToNotes)
       <&> (<?> [])
  where
    rights = bind $ right >>> \m -> try insert m mempty

    toNotes ((v, note), hs) = (set [Left note], (v, note))
                            : (toList hs <&> \h -> (set [Left note, Right h], harmonize h (v, note)))

    inputToNotes = proc (v, i) -> do
            harmonies <- rights <$> held -< (v, i)
            let notes = v <&> (,) <*> left i <&> (, harmonies) <&> toNotes
                startNotes = (Just <.>) . snd <$$> notes <?> []
            stopNotes <- notesOff -< (notes, i)
            id -< startNotes <> stopNotes

notesOff :: Stream Id (Maybe [(Set (Either Note Harmony), (Velocity, Note))], Either Note Harmony) Chord
notesOff = loop (barr offFunc) emptyMulti
    where
        offFunc (Nothing, off) m = ((Nothing,) <$>) <.> (multiremove off m <?> ([], m))
        offFunc (Just ons, _) m = ([], foldr (toList *** snd >>> barr multinsert) m ons)

test :: Test
test = $(testGroupGenerator)

prop_notes :: [(Velocity, Set Note)] -> Result
prop_notes = (\notes -> do
                        (v, chord) <- toList <$$> notes
                        [ ioPair (Just v) chord, ioPair Nothing chord ]
             )
         >>> streamTestEq seqLogic
    where
        -- input to expected output pairing for the stream
        ioPair v chord = ( Just (v, Chord chord)
                         , Id $ (v,) <$> chord
                         )

prop_harmony :: [(Velocity, ([Harmony], [Note]))] -> Result
prop_harmony = preprocess
           >>> (\inputs -> do
                    (v, (harmonies, notes)) <- inputs
                    let outNotes = set $ harmonize <$> harmonies <*> ((v,) <$> notes)
                        setNotes = (v,) <$> set notes
                    ioPair <$> [ ((Just v, Harmony harmonies), mempty)
                               , ((Just v, Chord notes), (Just <.>) <$> setNotes <> outNotes)
                               , ((Nothing, Harmony harmonies), off <$> outNotes)
                               , ((Nothing, Chord notes), off <$> setNotes)
                               ]
               )
           >>> streamTestEq (set <$> seqLogic)
    where
        reduceInput :: Ord a => [a] -> [a]
        reduceInput = take 16 >>> set >>> toList

        preprocess l = (reduceInput *** reduceInput) <$$> take 16 l
        ioPair = Just *** Id

        off = map2 $ \_-> Nothing
