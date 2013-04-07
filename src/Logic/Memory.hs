{-# LANGUAGE NoImplicitPrelude
           #-}
module Logic.Memory( memory
                   ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Id
import Sound.MIDI.Monad.Types

import Input

type Memory = (Maybe Tick, Song)

memory :: Stream Id (((Maybe Velocity, Input), Tick), Song) Song
memory = arr (fst.fst) &&& record >>> playback

record :: Stream Id (((Maybe Velocity, Input), Tick), Song) Memory
record = updater (map2 (map2 $ map2 $ barr isPressed) >>> barr state) (Nothing, mempty)
    where
        isPressed v inpt = v /= Nothing && isRecord inpt

        state ((pressed, dt), notes) m = if' pressed toggle
                                         $ updateTime dt $ append notes m

        append :: Song -> (Maybe Tick, Song) -> (Maybe Tick, Song)
        append notes (Just t, song) = (Just t, shift t notes <> song)
        append _ (Nothing, song) = (Nothing, song)

        shift :: Tick -> Song -> Song
        shift t notes = map2 (t +) <$> notes

        updateTime :: Tick -> (Maybe Tick, Song) -> (Maybe Tick, Song)
        updateTime dt (t, song) = (t <&> (+ dt), song)

        toggle :: (Maybe Tick, Song) -> (Maybe Tick, Song)
        toggle (Just _, song) = (Nothing, song)
        toggle _ = (Just 0, [])

playback :: Stream Id ((Maybe Velocity, Input), Memory) Song
playback = barr $ \(b, i) m -> mcond (b /= Nothing && isPlay i) (snd m) <?> []
