{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module Distrib.Butter.Lib.Protocol where
import Distrib.Butter.Lang

import Control.Concurrent.Forkable (ForkableMonad(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON(..), ToJSON(..))

data Result p = Reply p (State p)
              | NoReply (State p)
              | Terminate
              | Restart

class (ToJSON p, FromJSON p) => Protocol p where
  data State p
  type Context p :: * -> *
  setup   :: (MonadIO (Context p), ForkableMonad (Context p))
          => p -> Butter (Context p) (State p)
  handle  :: (MonadIO (Context p), ForkableMonad (Context p))
          => p -> State p -> Butter (Context p) (Result p)

start :: (MonadIO (Context p), ForkableMonad (Context p), Protocol p)
      => p -> Butter (Context p) (ProcessID)
start p =
  let server s = do
        (from,p')    <- receive
        result <- handle p' s
        case result of
          Reply p'' s' -> do
            send from p''
            server s'
          NoReply s' -> server s'
          Terminate  -> return ()
          Restart    -> do
            s' <- setup p'
            server s'
  in  do
    s <- setup p
    spawn $ server s

call :: (MonadIO (Context p), ForkableMonad (Context p), Protocol p)
     => ProcessID -> p -> Butter (Context p) p
call to p = do
  me <- self
  send to (me,p)
  receive

cast :: (MonadIO (Context p), ForkableMonad (Context p), Protocol p)
     => ProcessID -> p -> Butter (Context p) ()
cast to p = do
  me <- self
  send to (me,p)
