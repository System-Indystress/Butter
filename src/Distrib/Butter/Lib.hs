{-# LANGUAGE TypeFamilies #-}

module Distrib.Butter.Lib where
import Distrib.Butter.Lang

import Control.Concurrent.Forkable (ForkableMonad(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON(..), ToJSON(..))

class (ToJSON p, FromJSON p) => Protocol p where
  data State p
  setup   :: (MonadIO m, ForkableMonad m)
          => p -> Butter m (State p)
  handle  :: (MonadIO m, ForkableMonad m)
          => p -> State p -> Butter m (Maybe p,State p)

start :: (MonadIO m, ForkableMonad m, Protocol p)
      => p -> Butter m (ProcessID)
start p =
  let server s = do
        (from,p)    <- receive
        (result,s') <- handle p s
        case result of
          Nothing    -> return ()
          Just reply -> send from reply
        server s'
  in  do
    s <- setup p
    spawn $ server s

call :: (MonadIO m, ForkableMonad m, Protocol p) => ProcessID -> p -> Butter m p
call to p = do
  me <- self
  send to (me,p)
  receive

cast :: (MonadIO m, ForkableMonad m, Protocol p) => ProcessID -> p -> Butter m ()
cast to p = do
  me <- self
  send to (me,p)
