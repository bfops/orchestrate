{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Main.Input ( inputs
                  , bpm
                  ) where

import Prelude ()
import BasicPrelude hiding (map, mapM, mapM_, scanl, zip, zipWith, filter, sequence)

import Control.Eff
import Control.Eff.Lift as Eff (Lift, lift)
import Control.Lens
import Control.Monad.Trans as Trans (lift)
import Data.Conduit
import Data.Conduit.Extra
import Data.Conduit.List as Conduit
import Data.Foldable (traverse_)
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

-- TODO: Make key releases work properly across remaps.

-- | What controls what?
inputTranslations :: Monad m => [EventTranslator m ()]
inputTranslations = pianoMapper <> globalTranslations
    where
        globalTranslations = BasicPrelude.concat
            [ pitchHarmonyKeys
            , drumHarmonyKeys
            , drumMIDI
            , [1..9] >>= \i ->
                [ forever $ contiguously (matchKeyPress <$> [Key'Z, numKey i]) $ produce (Track Record i)
                , forever $ contiguously (matchKeyPress <$> [Key'X, numKey i]) $ produce (Track Play i)
                , forever $ contiguously (matchKeyPress <$> [Key'C, numKey i]) $ produce (Track Save i)
                , forever $ contiguously (matchKeyPress <$> [Key'V, numKey i]) $ produce (Track Load i)
                ]
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

        pianoMapper = BasicPrelude.concat [pianoMIDI, pianoKeys, violinHarmonies, remapToViolin]
            where
              piano :: Pitch -> Note
              piano = (, Instrument 0)

              pianoMIDI =
                  [0..120] <&> \n ->
                      forever $ match (note $ piano n) $ chord [piano n]

              pianoKeys =
                  [ forever $ matchCharKey 'A' $ chord [piano 48]
                  , forever $ matchCharKey 'S' $ chord [piano 50]
                  , forever $ matchCharKey 'D' $ chord [piano 52]
                  , forever $ matchCharKey 'F' $ chord [piano 53]
                  , forever $ matchCharKey 'G' $ chord [piano 55]
                  , forever $ matchCharKey 'H' $ chord [piano 57]
                  , forever $ matchCharKey 'J' $ chord [piano 59]
                  , forever $ matchCharKey 'K' $ chord [piano 60]
                  ]

              violinHarmonies =
                  [0..9] <&> \i ->
                    forever
                      $ matchCharKey ("QWERTYUIOP" !! i)
                      $ harmony
                      $ Harmony { changeVelocity = Just (-16), changeInstrument = Just $ Instrument 40, changePitch = Just $ DeltaPitch $ fromIntegral i }

              remapToViolin = [ forever $ matchKeyPress Key'Semicolon $ remap $ violinMapper <> globalTranslations ]

        violinMapper = BasicPrelude.concat [violinMIDI, violinKeys, pianoHarmonies, remapToPiano]
            where
              violin :: Pitch -> Note
              violin = (, Instrument 40)

              violinMIDI = 
                  [0..23] <&> \i ->
                      forever $ match (note (36 + i, Instrument 0)) $ chord [(48 + i, Instrument 40)]

              violinKeys =
                  [0..7] <&> \i ->
                      forever $ matchCharKey ("ASDFGHJK" !! i) $ chord [violin $ [48, 50, 52, 53, 55, 57, 59, 60] !! i]

              pianoHarmonies =
                  [0..9] <&> \i ->
                      forever
                        $ matchCharKey ("QWERTYUIOP" !! i)
                        $ harmony
                        $ Harmony { changeVelocity = Just 8, changeInstrument = Just $ Instrument 0, changePitch = Just $ DeltaPitch $ fromIntegral i }

              remapToPiano = [ forever $ matchKeyPress Key'Semicolon $ remap $ pianoMapper <> globalTranslations ]

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
