{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Polysemy.Acquire
  ( with,
  )
where

import qualified Control.Exception as E
import Data.Acquire.Internal (Acquire (..), Allocated (Allocated), ReleaseType (ReleaseException, ReleaseNormal))
import Polysemy (Member, Sem)
import Polysemy.Final (Final, withWeavingToFinal)

with :: forall tok r a. Member (Final IO) r => Acquire tok -> (tok -> Sem r a) -> Sem r a
with (Acquire f) act =
  withWeavingToFinal $ \state weave inspect ->
    E.mask $ \restore -> do
      Allocated x free <- f restore
      res <- restore (weave $ act x <$ state) `E.onException` free ReleaseException
      free $ case inspect res of
        Nothing -> ReleaseException
        Just _ -> ReleaseNormal
      return res
