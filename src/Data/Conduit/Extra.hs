{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Extra ( resumable
                          , exhaustInput
                          , awaitOr
                          , match
                          , contiguously
                          , yieldM
                          , untilTrue
                          , zipSourceWith
                          , fmapMaybe
                          ) where

import Prelude ()
import BasicPrelude

import Data.Conduit
import Data.Conduit.Internal (ResumableConduit (..), ConduitM (..), Pipe (..))

resumable :: Monad m => Conduit i m o -> ResumableConduit i m o
resumable c = ResumableConduit c (return ())

exhaustInput ::
    Monad m =>
    Source m i ->
    ResumableConduit i m o ->
    m (ResumableConduit i m o, [o])
exhaustInput
    = \(ConduitM left0) (ResumableConduit (ConduitM right0) final0) -> goRight final0 left0 right0 []
  where
    goRight final left right result =
        case right of
            HaveOutput next final' o -> goRight (final' >> final) left next (o:result)
            NeedInput rp rc -> goLeft rp rc final left result
            Done () -> return (ResumableConduit (ConduitM right) final, reverse result)
            PipeM mp -> do
                next <- mp
                goRight final left next result
            Leftover next i -> goRight final (HaveOutput left (return ()) i) next result

    goLeft rp rc final left result =
        case left of
            HaveOutput left' final' o -> goRight (final >> final') left' (rp o) result
            NeedInput _ _ -> error "NeedInput Source"
            Done () -> return (ResumableConduit (ConduitM $ NeedInput rp rc) final, reverse result)
            PipeM mp -> do
                next <- mp
                goLeft rp rc final next result
            Leftover _ _ -> error "Leftover Source"

awaitOr :: Monad m => r -> (i -> ConduitM i o m r) -> ConduitM i o m r
awaitOr r f = await >>= maybe (return r) f

match :: Monad m => (i -> Bool) -> (i -> ConduitM i o m Bool) -> ConduitM i o m Bool
match p f = awaitOr False $ \i -> if p i
                                  then True <$ f i
                                  else return False

contiguously :: (Functor m, Monad m) => [((x -> m Bool) -> m Bool)] -> m () -> m Bool
contiguously l c = foldr (\f p -> f $ \_-> p) (True <$ c) l

yieldM :: Show a => Monad m => m a -> Conduit i m a
yieldM m = lift m >>= yield

untilTrue :: Monad m => m Bool -> m ()
untilTrue c = c >>= \b -> if b then return () else untilTrue c

zipSourceWith :: Monad m => (a -> b -> c) -> Source m a -> Source m b -> Source m c
zipSourceWith f sa sb = getZipSource $ f <$> ZipSource sa <*> ZipSource sb

fmapMaybe :: Monad m => ConduitM i o m r -> ConduitM (Maybe i) (Maybe o) m r
fmapMaybe = \(ConduitM right0) -> ConduitM $ go Nothing right0
  where
    go :: Monad m => Maybe i -> Pipe i i o u m r -> Pipe (Maybe i) (Maybe i) (Maybe o) u m r
    go result = \case
        HaveOutput next final o -> HaveOutput (go result next) final (o <$ result)
        NeedInput rp rc -> NeedInput
                              (\m -> maybe
                                      (HaveOutput (go m $ NeedInput rp rc) (return ()) Nothing)
                                      (go m . rp)
                                      m
                              )
                              (go result . rc)
        Done r -> Done r
        PipeM mp -> PipeM $ mp >>= return . go result
        Leftover next i -> Leftover (go result next) (Just i)
