{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , FlexibleContexts
           , TemplateHaskell
           #-}
module Main.Input( inputs
                 ) where

import Summit.Control.Stream as S
import Summit.Data.Id
import Summit.Data.Map
import Summit.Impure
import Summit.IO
import Summit.Prelewd
import Summit.Template.MemberTransformer

import Control.Eff
import Control.Eff.Lift as E
import Control.Stream.Util
import Data.Char (Char)
import Data.Trie

import Wrappers.Events

import Sound.MIDI

import Main.Graphics

import Input

sequence2 :: (Mappable (->) f a (Either a b), Mappable (->) f b (Either a b))
          => Either (f a) (f b) -> f (Either a b)
sequence2 = either (map Left) (map Right)

mswitch :: Functor m2 => (m1 (a, Stream m1 r a) -> m2 (b, Stream m1 r a)) -> Stream m1 r a -> Stream m2 r b
mswitch f s = Stream $ \r -> mswitch f <$$> f (s $< r)

defaultVelocity :: Velocity
defaultVelocity = 64

-- | What controls what?
mapInput :: InputMap
mapInput = fromMap (map Left <.> fromList ( harmonyButtons
                                         <> recordButtons
                                         <> saveButtons
                                         <> loadButtons
                                          ))
        <> pianoMap
        <> drumMIDI
    where
        harmonyButtons = map ((:[]) . KeyButton . CharKey *** Harmony)
                       $ [(numChar i, [(Nothing, (Nothing, Right $ fromInteger i))]) | i <- [1..9]]
                      <> [('[', [(Just 32, (Just Percussion, Left 42))])]

        tracks = [1..9]

        recordButtons = map (map2 $ map (KeyButton . CharKey))
                      $ do i <- tracks
                           let c = numChar i
                           [(['Z', c], Track Record i), (['X', c], Track Play i)]

        saveButtons = tracks
                  <&> \i -> (KeyButton . CharKey <$> ['C', numChar i], Track Save i)

        loadButtons = tracks
                  <&> \i -> (KeyButton . CharKey <$> ['V', numChar i], Track (Load ()) i)

numChar :: Integer -> Char
numChar i = "0123456789" ! i

piano :: Pitch -> Note
piano = (, Instrument 0)

pianoMIDI :: InputMap
pianoMIDI = fromMap $ fromList $ ((:[]) . Right *** Chord) <$> [(piano n, [piano n]) | n <- [0..120]]

drumMIDI :: InputMap
drumMIDI = fromMap $ fromList $ ((:[]) . Right *** Chord) <$> [(drum n, [drum n]) | n <- [35..81]]
    where drum = (, Percussion)

pianoMap :: InputMap
pianoMap = fromMap (fromList $ noteButtons <> harmonyButtons <> remapButtons) <> pianoMIDI
    where
        noteButtons = map ((:[]) . Left . KeyButton . CharKey *** Chord . map piano)
            [("ASDFGHJK" ! i, [[48, 50, 52, 53, 55, 57, 59, 60] ! i]) | i <- [0..7]]

        harmonyButtons = map ((:[]) . Left . KeyButton . CharKey *** Harmony)
            [("QWERTYUIOP" ! i, [(Just (-16), (Just $ Instrument 40, Right $ fromInteger i))]) | i <- [0..9]]

        remapButtons = map ((:[]) . Left . KeyButton . CharKey *** Remap)
            [ (';', violinMap)
            ]

violinMap :: InputMap
violinMap = fromMap
          $ (map Left <.> fromList (noteButtons <> harmonyButtons <> remapButtons))
         <> (map Right <.> fromList violinMIDI)
    where
        violin = (, Instrument 40)

        noteButtons = map ((:[]) . KeyButton . CharKey *** Chord . map violin)
            [("ASDFGHJK" ! i, [[48, 50, 52, 53, 55, 57, 59, 60] ! i]) | i <- [0..7]]

        harmonyButtons = map ((:[]) . KeyButton . CharKey *** Harmony)
            [("QWERTYUIOP" ! i, [(Just 8, (Just $ Instrument 0, Right $ fromInteger i))]) | i <- [0..9]]

        remapButtons = map ((:[]) . KeyButton . CharKey *** Remap)
            [ (';', pianoMap)
            ]

        violinMIDI = map ((:[]) *** Chord)
            [((36 + i, Instrument 0), [(48 + i, Instrument 40)]) | i <- [0..23]]

data Context = Context
             { allMap        :: InputMap                 -- ^ The total mapping of events to inputs
             , currentMap    :: InputMap                 -- ^ The current mapping of events to inputs
             , offMap        :: Map UnifiedEvent Input   -- ^ The current mapping of toggle-off events to inputs
             }

$(memberTransformers ''Context)

inputs :: MIDI env => Stream (Eff env) () [(Maybe Velocity, Input)]
inputs = (Left <$$> buttons) <&> (<>) <*> (Right <$$> notes) >>> identify convertAll

buttons :: SetMember Lift (Lift IO) env => Stream (Eff env) () [(Bool, Button)]
buttons = mswitch E.lift
        $ events
      >>> S.lift (arr $ traverse toButton)
      <&> concat
    where
        toButton CloseEvent = empty
        toButton (ResizeEvent s) = resize s $> []
        toButton (ButtonEvent b s) = return [(s == Press, b)]
        toButton _ = return []

notes :: MIDI env => Stream (Eff env) () [(Maybe Velocity, Note)]
notes = S.lift $ arr $ \_-> midiIn

convertAll :: Stream Id [Either (Bool, Button) (Maybe Velocity, Note)] [(Maybe Velocity, Input)]
convertAll = map (boolToVelocity <&> sequence2 >>> convert) <&> mapMaybe id
    where
        boolToVelocity = arr $ map2 $ map2 (`mcond` defaultVelocity)

convert :: Stream Id (Maybe Velocity, UnifiedEvent) (Maybe (Maybe Velocity, Input))
convert = loop (barr convertFunc) (Context mapInput mapInput mempty) >>> bind hold
    where
        convertFunc (Nothing, e) cxt = (do
                                        (i, off) <- remove e $ offMap cxt
                                        return ( Just (Nothing, i)
                                               , cxt { offMap = off }
                                               )
                                       ) <?> (Nothing, cxt)
        convertFunc (Just v, e) cxt = case trie e $ currentMap cxt of
                                        Nothing -> (Nothing, reset cxt)
                                        Just (Value i) -> case fromRemap i of
                                                Just r -> (Nothing, reset $ allMap' (r <>) cxt)
                                                _ -> ( Just (Just v, i)
                                                     , offMap' (insertWith (error "double-pressed button") e i)
                                                     $ reset
                                                     $ cxt
                                                     )
                                        Just c -> (Nothing, cxt { currentMap = c })

        reset = currentMap' =<< \cxt _-> allMap cxt
