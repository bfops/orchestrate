{-# LANGUAGE NoImplicitPrelude
           #-}
module Logic.Memory( memory
                   ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Map
import Storage.Id
import Sound.MIDI.Monad.Types

import Input

type Memory = Map Track (Maybe Tick, Song)

memory :: Stream Id (((Maybe Velocity, Input), Tick), Song) Song
memory = arr (fst.fst) &&& record >>> playback

record :: Stream Id (((Maybe Velocity, Input), Tick), Song) Memory
record = updater (map2 (map2 $ map2 $ barr whichTrack) >>> barr state) mempty
    where
        whichTrack pressed inpt = pressed >> fromRecord inpt

        state ((track, dt), notes) m = try (alter $ Just . toggle) track
                                     $ updateTime dt . append notes <$> m

        append :: Song -> (Maybe Tick, Song) -> (Maybe Tick, Song)
        append notes (Just t, song) = (Just t, shift t notes <> song)
        append _ (Nothing, song) = (Nothing, song)

        shift :: Tick -> Song -> Song
        shift t notes = map2 (t +) <$> notes

        updateTime :: Tick -> (Maybe Tick, Song) -> (Maybe Tick, Song)
        updateTime dt (t, song) = (t <&> (+ dt), song)

        toggle :: Maybe (Maybe Tick, Song) -> (Maybe Tick, Song)
        toggle (Just (Just _, song)) = (Nothing, song)
        toggle _ = (Just 0, [])

playback :: Stream Id ((Maybe Velocity, Input), Memory) Song
playback = barr $ \(b, i) m -> (b >> fromPlay i >>= (`lookup` m)) <&> snd <?> []
