module Translator ( Translator
                  , Translation (..)
                  , translator
                  ) where

import Prelude ()
import BasicPrelude

import Control.Lens
import Data.Conduit
import Data.Conduit.Extra

data Translation m i o
    = ResetTranslators
    | AddTranslator (Translator m i o ())
    | Produce o

type Translator m i o = ConduitM i (Translation m i o) m

-- TODO: Remove conduits upon exhaustion.
stepAll :: (Applicative m, Monad m) => i -> [ResumableConduit i m o] -> m ([ResumableConduit i m o], [o])
stepAll i conduits
    = traverse (exhaustInput (yield i)) conduits
  <&> unzip
  <&> second BasicPrelude.concat

translator :: (Applicative m, Monad m) => [Translator m i o ()] -> Conduit i m o
translator = inner . fmap resumable
  where
    inner ts = do
      mi <- await
      case mi of
        Nothing -> return ()
        Just i -> do
            (ts', outs) <- lift (stepAll i ts)
            foldM flatten ts' outs >>= inner

    -- "flatten" @`Translation` m i o@s into `Conduit`s yielding just @o@s, by handling the other cases.
    flatten ts (Produce o) = ts <$ yield o
    flatten _ ResetTranslators = return mempty
    flatten ts (AddTranslator dt) = return (resumable dt : ts)
