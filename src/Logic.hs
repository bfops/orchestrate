{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , MultiParamTypeClasses
           , FlexibleInstances
           , FunctionalDependencies
           , UndecidableInstances
           #-}
module Logic ( noteLogic
             ) where

import Prelewd

import Impure

import Control.Stream
import Data.Int
import Sound.MIDI.Monad
import Storage.Id
import Storage.Map
import Subset.Num

import Input

class Propogate r a b where
    propogate :: r -> a -> b

instance Propogate r a (r, a) where
    propogate = (,)

instance (Propogate r a b, Propogate r x y) => Propogate r (a, x) (b, y) where
    propogate r = propogate r *** propogate r

noteLogic :: Stream Id (Bool, Input) [(Bool, (Tick, Note))]
noteLogic = arr (map fromNote) &&& harmonies
        >>> loop (barr toNotes) mempty
        >>> arr ((0,) <$$>)

harmonies :: Stream Id (Bool, Input) [Int16]
harmonies = arr (map fromHarmony) >>> updater (barr newInputMap) initHarmonies >>> arr keys
    where
        initHarmonies = singleton 0 (1 :: Positive Integer)

        newInputMap (_, Nothing) m = m
        newInputMap (True, Just shift) m = insertWith (+) shift 1 m
        newInputMap (False, Just shift) m = modify (\v -> toPos $ fromPos v - 1) shift m <?> m

toNotes :: ((Bool, Maybe Note), [Int16]) -> Map Note [Note] -> ([(Bool, Note)], Map Note [Note])
toNotes ((_, Nothing), _) hmap = ([], hmap)
toNotes ((b, Just note), hs) hmap = let (v, hmap') = remove note hmap <?> error "double removal"
                                    in ( (b,) <$> iff b company v
                                       , iff b (insert note company hmap) hmap'
                                       )
    where
        company = hs <&> \dp -> pitch' (fromIntegral . (dp +) . fromIntegral) note