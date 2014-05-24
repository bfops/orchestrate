{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Logic ( logic
             ) where

import Prelude ()
import BasicPrelude as Base

import Control.Eff
import Control.Eff.Lift as Eff
import Control.Concurrent.STM
import Control.Lens (view, set, over, (<&>))
import Control.Monad.Trans.Class as Trans
import Data.Conduit
import Data.Conduit.List as Conduit
import Data.HashMap.Strict as HashMap
import Data.Refcount
import Data.Text (unpack)
import Data.Vector as Vector
import Sound.MIDI

import Data.Conduit.Extra
import Input
import TrackMemory

#if MIN_VERSION_base(4,7,0)
#else
#define Typeable Typeable1
#endif

trackFile :: Track -> FilePath
trackFile t = fromString $ unpack $ "track" <> show t

-- | "Hold" inputs by counting the number of @Just@ and @Nothing@ (on and off)
-- signals for a given @a@, and only send the off signal corresponding to the
-- last on signal.
hold :: (MonadIO m, Hashable a, Eq a) => Conduit (a, Maybe b) m (a, Maybe b)
hold = inner mempty
  where
    inner held = do
      awaitOr () $ \case
        e@(note, Nothing) -> case removeRef note held of
            Nothing -> do
                putStrLn $ "off signal without corresponding on signal"
                inner held
            Just (removed, held') -> do
                if removed
                then yield e
                else return ()
                inner held'
        e@(note, Just _) -> do
            yield e
            inner (insertRef note held)

liftSTMConduit :: (Typeable m, MonadIO m, SetMember Lift (Lift m) r) => STM a -> ConduitM i o (Eff r) a
liftSTMConduit = Trans.lift . liftIO . atomically

logic :: SetMember Lift (Lift IO) env => Conduit Input (Eff env) (Note, Maybe Velocity)
logic = do
    tracks <- liftSTMConduit $ newTVar mempty
    harmonies <- liftSTMConduit $ newTVar mempty
    awaitForever (stepLogic tracks harmonies) =$= hold

initialTrackMemory :: TrackMemory
initialTrackMemory
    = TrackMemory
        { _trackData = Vector.empty
        , _recording = False
        , _playState = Nothing
        }

modifyExtant ::
    MonadIO m =>
    TVar MemoryBank ->
    Track ->
    (TrackMemory -> TrackMemory) ->
    m ()
modifyExtant tracks t f
    = liftIO
    $ atomically
    $ modifyTVar tracks
    $ \memory -> let memory' = if not $ HashMap.member t memory
                               then HashMap.insert t initialTrackMemory memory
                               else memory
                 in over (track t) f memory'

sideEffect :: Functor f => (a -> f ()) -> a -> f a
sideEffect f a = a <$ f a

modifyTVarWith :: TVar a -> (a -> (b, a)) -> STM b
modifyTVarWith t f = do
    a <- readTVar t
    let (b, a') = f a
    writeTVar t a'
    return b

-- TODO: when harmonies are released, release associated notes properly.
stepLogic ::
    SetMember Lift (Lift IO) env =>
    TVar MemoryBank ->
    TVar (Refcount Harmony) ->
    Input ->
    Conduit i (Eff env) (Note, Maybe Velocity)
stepLogic tracks harmoniesVar
      = \e -> do
          justProcessInput e
              -- Append all the notes that are produced to all the recording tracks.
              =$= Conduit.mapM (sideEffect $ \(note, v) -> onRecordingTracks (`snoc` NoteOutput note v))
          onRecordingTracks (snocIfTimestep e)
  where
    -- just handle the immediate "consequences" of the input,
    -- with no real attention paid to "long-term" effects.
    justProcessInput e = case e of
        NoteInput note mv -> do
          yield (note, mv)
          harmonies <- liftSTMConduit $ readTVar harmoniesVar
          Base.forM_ (keys $ unRefcount harmonies) $ \h -> do
            yield $ harmonize h (note, mv)

        HarmonyInput harmony isOn -> liftSTMConduit $ do
          modifyTVar' harmoniesVar $ \harmonies ->
              if isOn
              then insertRef harmony harmonies
              else fromMaybe harmonies $ deleteRef harmony harmonies
        
        Track Record t -> do
          modifyExtant tracks t toggleRecording

        Track (Play l) t -> do
          modifyExtant tracks t (togglePlaying l)

        Track Save t -> do
          dat <- liftIO $ atomically $ readTVar tracks <&> view (track t) <&> view trackData
          Trans.lift $ liftIO $ writeFile (trackFile t) $ show dat

        Track Load t -> do
          dat <- read <$> Trans.lift (liftIO $ readFile (trackFile t))
          modifyExtant tracks t $ set trackData dat

        Timestep dt -> do
          outs <- liftIO $ atomically $ modifyTVarWith tracks $ \mem -> let
                    (outs, mem')
                        = Base.unzip
                        $ fmap (\(t, (o, d)) -> (o, (t, d)))
                        $ HashMap.toList
                        $ advancePlayBy dt <$> mem
                    in (outs, HashMap.fromList mem')
          yield (Base.concat outs) =$= Conduit.concat

    onRecordingTracks f
      = liftIO
      $ atomically
      $ modifyTVar tracks
      $ fmap
      $ \t ->
          if view recording t
          then over trackData f t
          else t

    snocIfTimestep (Timestep dt) v
        -- don't pad dead time before notes
        = if Vector.null v
          then v
          else snoc v (RestOutput dt)
    snocIfTimestep _ t = t

    -- TODO: When recording finishes, release all started notes.
    -- TODO: Don't allow release events without corresponding push events to be recorded/played back.
    toggleRecording t
        = let t' = over recording not t in
          if view recording t'
          then
            let t'' = set trackData Vector.empty t' in
            if view isPlaying t''
            then togglePlaying (error "togglePlaying on") t''
            else t''
          else t'

    -- this doesn't play nicely with track looping
    stripSuffixRests v
      = let len = Vector.length v
        in
          if len == 0
          then v
          else case Vector.last v of
                RestOutput _ -> stripSuffixRests $ Vector.take (len - 1) v
                _ -> v

    togglePlaying l t
        = let t' = over playState (maybe (Just (0, 0, l)) (\_-> Nothing)) t in
          if view isPlaying t' && view recording t'
          then toggleRecording t'
          else t'

    advancePlayBy = \dt t -> case view playState t of
          Nothing -> ([], t)
          Just (start, remaining, l) -> let
                (playState', outs) = continueTo l (dt + remaining) start (view trackData t) []
              in (Base.reverse outs, set playState playState' t)
      where
        continueTo l t start v outs
            = if start < Vector.length v
              then case v Vector.! start of
                    RestOutput dt ->
                      if dt < t
                      then continueTo l (t - dt) (start + 1) v outs
                      else (Just (start, t, l), outs)
                    NoteOutput note vel -> continueTo l t (start + 1) v ((note, vel) : outs)
              else case l of
                    Once -> (Nothing, outs)
                    -- TODO: if we try to play an empty track, this asplodes - FIX
                    Loop -> continueTo l t 0 v outs
