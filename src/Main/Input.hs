{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Main.Input ( inputs
                  , bpm
                  ) where

import Prelude ()
import BasicPrelude hiding (map, mapM, mapM_, scanl, zip, zipWith, filter, sequence)

import Control.Applicative
import Control.Eff
import Control.Eff.Lift as Eff (Lift, lift)
import Control.Lens
import Control.Monad.Trans as Trans (lift)
import Data.Conduit
import Data.Conduit.Extra
import Data.Conduit.List as Conduit
import Data.Foldable (traverse_)
import Data.List as List
import Data.StateVar (get)
import Sound.MIDI
import Wrappers.Events
import Wrappers.GLFW

import Input
import Translator
import Translator.Keyboard
import Main.Graphics (resize)

pressVelocity :: Velocity
pressVelocity = 64

granularity :: Tick
granularity = 2

-- | Beats per minute
bpm :: Num a => a
bpm = 60

chord :: Monad m => [Note] -> UnifiedEvent -> EventTranslator m Bool
chord notes e = do
    let v = view (velocity pressVelocity) e
    forM_ notes $ \n -> yield $ Produce $ NoteInput n v
    return True

harmony :: Monad m => Harmony -> UnifiedEvent -> EventTranslator m Bool
harmony h e = do
    let isOn = isJust $ view (velocity pressVelocity) e
    yield $ Produce $ HarmonyInput h isOn
    return True

note :: Note -> UnifiedEvent -> Bool
note (instr, pitch) (Midi (i, p) _) = instr == i && pitch == p
note _ _ = False

remap :: Monad m => [EventTranslator m ()] -> UnifiedEvent -> EventTranslator m Bool
remap l e = do
    let isOn = isJust $ view (velocity pressVelocity) e
    if isOn
    then do
      yield ResetTranslators
      traverse_ (yield . AddTranslator) l
      return True
    else return True

produce :: Monad m => Input -> EventTranslator m ()
produce = yield . Produce

octave :: Pitch
octave = 12

-- TODO: Make key releases work properly across remaps.

-- | What controls what?
inputTranslations :: Monad m => [EventTranslator m ()]
inputTranslations = pianoMapper 0 <> globalTranslations
    where
        trackCommands
            = liftA2 (,) [1..9]
                [ (Key'Z, Record)
                , (Key'X, Play Once)
                , (Key'C, Save)
                , (Key'V, Load)
                , (Key'B, Play Loop)
                ]

        globalTranslations = BasicPrelude.concat
            [ pitchHarmonyKeys
            , drumHarmonyKeys
            , drumMIDI
            , trackCommands <&> \(i, (key, cmd)) ->
                forever $ contiguously (matchKeyPress <$> [key, numKey i]) $ produce (Track cmd i)
            ]

        pitchHarmonyKeys =
            [1..9] <&> \i ->
                forever
                  $ matchKeyPressAndRelease (numKey i)
                  $ harmony
                  $ emptyHarmony { changePitch = Just $ DeltaPitch i }

        drumHarmonyKeys =
            [ forever
                $ matchKeyPressAndRelease Key'LeftBracket
                $ harmony
                $ Harmony { changeVelocity = Just 64, changeInstrument = Just Percussion, changePitch = Just $ Absolute 42 }
            ]

        drumMIDI =
            [35..81] <&> \n ->
                forever $ match (note $ drum n) $ chord [drum n]
          where drum = (, Percussion)

        -- The changes in pitch along the absolute-pitch keys.
        keySteps = [0, 2, 4, 5, 7, 9, 11, 12]

        pianoMapper root
            = BasicPrelude.concat [pianoMIDI, pianoKeys, violinHarmonies, remapToViolin, pitchShiftKeys]
            where
              piano :: Pitch -> Note
              piano = (, Instrument 0)

              pianoMIDI =
                  [0..120] <&> \n ->
                      forever $ match (note $ piano n) $ chord [piano $ n + root]

              pianoKeys =
                zip "ASDFGHJK" keySteps <&> \(key, step) ->
                  forever $ matchCharKey key $ chord [piano $ 4 * octave + root + step]

              violinHarmonies =
                  [0..9] <&> \i ->
                    forever
                      $ matchCharKey ("QWERTYUIOP" !! i)
                      $ harmony
                      $ Harmony
                      { changeVelocity = Just (-16)
                      , changeInstrument = Just $ Instrument 40
                      , changePitch = Just $ DeltaPitch $ fromIntegral i
                      }

              remapToViolin = [ forever $ matchKeyPress Key'Semicolon $ remap $ violinMapper root <> globalTranslations ]

              pitchShiftKeys =
                  [ forever $ matchKeyPress Key'M $ remap $ pianoMapper (root + 1) <> globalTranslations
                  , forever $ matchKeyPress Key'N $ remap $ pianoMapper (root - 1) <> globalTranslations
                  ]

        violinMapper root = BasicPrelude.concat [violinMIDI, pianoMIDI, violinKeys, pianoHarmonies, remapToPiano, pitchShiftKeys]
            where
              violin :: Pitch -> Note
              violin = (, Instrument 40)

              violinMIDI = 
                  [36..59] <&> \i ->
                      forever $ match (note (i, Instrument 0)) $ chord [violin $ 2 * octave + i + root]

              pianoMIDI =
                  [60..120] <&> \i ->
                      forever $ match (note (i, Instrument 0)) $ chord [(i + root, Instrument 0)]

              violinKeys =
                  zip [0..7] keySteps <&> \(i, step) ->
                      forever $ matchCharKey ("ASDFGHJK" !! i) $ chord [violin $ 4 * octave + step + root]

              pianoHarmonies =
                  [0..9] <&> \i ->
                      forever
                        $ matchCharKey ("QWERTYUIOP" !! i)
                        $ harmony
                        $ Harmony { changeVelocity = Just 8, changeInstrument = Just $ Instrument 0, changePitch = Just $ DeltaPitch $ fromIntegral i }

              remapToPiano = [ forever $ matchKeyPress Key'Semicolon $ remap $ pianoMapper root <> globalTranslations ]

              pitchShiftKeys =
                  [ forever $ matchKeyPress Key'M $ remap $ violinMapper (root + 1) <> globalTranslations
                  , forever $ matchKeyPress Key'N $ remap $ violinMapper (root - 1) <> globalTranslations
                  ]

-- TODO: sleep between yields
timesteps :: SetMember Lift (Lift IO) env => Source (Eff env) (Maybe Input)
timesteps = forever (yieldM (liftIO $ get time))
      $= Conduit.map toTick
      $= void (mapAccum delta Nothing)
      $= Conduit.map (\d -> if d > 0 then Just (Timestep d) else Nothing)
  where
    delta t prev = (Just t, maybe 0 (t -) prev)

    -- TODO: Investigate overflow scenarios
    toTick :: Double -> Tick
    toTick t = floor (t * bpm * 96 / 60 / fromIntegral granularity) * granularity

inputs :: MIDI env => Source (Eff env) Input
inputs = zipSourceWith (\a b -> [a, b]) timesteps
          (  forever (yieldM $ (<>) <$> buttons <*> notes)
          $= Conduit.map (fmap Just)
          $= Conduit.map (Nothing:)
          $= Conduit.concat
          $= fmapMaybe 
              (  filterIOEvents
              $= Conduit.mapM (\x -> x <$ Eff.lift (putStrLn $ fromString ("input to translator ") <> show x))
              $= translator inputTranslations
              $= Conduit.mapM (\x -> x <$ Eff.lift (putStrLn $ fromString ("output from translator ") <> show x))
              )
          )
        $= Conduit.concat
        $= Conduit.catMaybes
  where
      filterIOEvents = awaitOr () $ \e -> case e of
              Event CloseEvent -> Trans.lift $ Eff.lift $ putStrLn $ fromString "CloseEvent received"
              Event (ResizeEvent s) -> do
                  Trans.lift (Eff.lift $ resize s)
                  filterIOEvents
              _ -> yield e >> filterIOEvents

      buttons :: SetMember Lift (Lift IO) env => Eff env [UnifiedEvent]
      buttons = fmap Event <$> Eff.lift popEvents

      notes :: MIDI env => Eff env [UnifiedEvent]
      notes = fmap (\(v, n) -> Midi n v) <$> midiIn
