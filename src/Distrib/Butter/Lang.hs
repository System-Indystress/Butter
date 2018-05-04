{-#LANGUAGE KindSignatures, GADTs, DeriveGeneric, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, PatternSynonyms, FlexibleInstances #-}
module Distrib.Butter.Lang where

import Control.Monad.Trans.Except
import Data.Text
import Data.Aeson
import GHC.Generics
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import qualified Data.Map as M
import Data.Map (Map(..))
import Distrib.TH
import Control.Monad.Free
import Network.Simple.TCP
import Control.Concurrent.Forkable
import Control.Concurrent (threadDelay)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..),(<|),(|>))

import Debug.Trace (trace)


data ProcessID = PID {machineID :: Text, processID :: Int}
  deriving (Show, Eq, Generic)

instance ToJSON (ProcessID) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON (ProcessID)

data Action (m :: * -> *) next where
    Lift    :: m a
            -> (a -> next)
            -> Action m next
    Connect :: Text
            -> Int
            -> next
            -> Action m next
    Spawn   :: Butter m ()
            -> (ProcessID -> next)
            -> Action m next
    Send    :: ProcessID
            -> Value
            -> next
            -> Action m next
    Receive :: (Value -> next)
            -> Action m next
    Friends :: ([Text] -> next)
            -> Action m next
    Self    :: (ProcessID -> next)
            -> Action m next
    Alive   :: ProcessID
            -> (Bool -> next)
            -> Action m next


data Internal =
  Internal { machine :: (Text,Maybe Socket)
           , procs   :: [(Int)]
           , fresh   :: Int
           , friends :: [(Text, Socket)]
           , mail    :: Seq (ProcessID, ProcessID, Value)
           }

instance Functor (Action m) where
  fmap f (Spawn body returnPID)   = (Spawn body (f . returnPID))
  fmap f (Send pid msg next)      = (Send pid msg $ f next)
  fmap f (Receive returnMSG)      = (Receive $ f . returnMSG)
  fmap f (Connect host port next) = (Connect host port $ f next)
  fmap f (Friends returnFriends)  = (Friends $ f . returnFriends)
  fmap f (Lift ma returnA)        = (Lift ma $ f . returnA)
  fmap f (Self returnMe)          = (Self $ f . returnMe)
  fmap f (Alive pid returnB)      = (Alive pid $ f . returnB)

type Butter m a = Free (Action m) a

instance (MonadIO m) => MonadIO (Free (Action m)) where
  liftIO = lift . liftIO

connect :: Text -> Int -> Butter m ()
connect host port = (Free (Connect host port $ Pure ()))

spawn :: Butter m () -> Butter m ProcessID
spawn body = (Free (Spawn body (\i -> Pure  i)))

self ::  Butter m ProcessID
self = (Free (Self (\i -> Pure  i)))

send :: (ToJSON a) => ProcessID -> a -> Butter m ()
send to msg = (Free (Send to (toJSON msg) $ Pure ()))

receive :: (MonadIO m, FromJSON a) => Butter m a
receive =
  (Free $ Receive $ \msg ->
            case fromJSON msg of
              Error _   -> do
                lift $ liftIO $ yield
                me <- self
                (Free $ Send me msg (Pure ()))
                receive
              Success a -> return a)

lift :: m a -> Butter m a
lift ma = Free (Lift ma $ \a -> Pure a)

alive :: ProcessID -> Butter m (Bool)
alive pid = (Free $ Alive pid $ \b -> Pure b)

friend :: Text -> ProcessID
friend host = PID host 0



spreadLocal :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a)
            => Butter m a -> m a
spreadLocal = spread "local" Nothing

spread :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a)
       => Text -> Maybe Int -> Butter m a -> m a
spread host mport actor =
  let state :: (MonadIO m) => m Internal
      state = do
        msock <- case mport of
                   Just port -> do
                      (sock, _) <- liftIO $ bindSock HostAny (show port)
                      return $ Just sock
                   Nothing   -> return Nothing

        return $
          Internal { machine = (host,msock)
                   , procs   = [0]
                   , fresh   = 1
                   , friends = []
                   , mail    = Seq.empty
                   }

      eval :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a) => Int -> TVar Internal -> Butter m a -> m a
      eval me stateVar (Pure a) = do
        liftIO $ atomically $ do
          i@(Internal { procs   = ps }) <- readTVar stateVar
          writeTVar stateVar $ i {procs = Prelude.filter (\n -> n /= me) ps}
        return a
      eval me stateVar (Free (Alive (PID _ n) returnB)) = do
        i@(Internal {procs = ps}) <- liftIO $ readTVarIO stateVar
        eval me stateVar $ returnB $ elem n ps
      eval me stateVar (Free (Lift ma returnA)) = do
        a <- ma
        eval me stateVar $ returnA a
      eval me stateVar (Free (Friends returnFriends)) = do
        i@(Internal { machine = (h,sock)
                    , procs   = ps
                    , friends = fs
                    , fresh   = f
                    , mail    = m
                    }) <- liftIO $ readTVarIO stateVar
        eval me stateVar $ returnFriends $ Prelude.map fst fs
      eval me stateVar (Free (Connect name port rest)) = do
        eval me stateVar rest
      eval me stateVar (Free (Spawn body returnPID)) = do
        (h,f) <-
          liftIO $ atomically $ do
            i@(Internal { machine = (h,_)
                        , procs   = ps
                        , fresh   = f
                        }) <- readTVar stateVar
            writeTVar stateVar $ i {fresh = f + 1, procs = (f+1):ps}
            return (h,f+1)
        let pid = PID h (f)
        forkIO $ eval (f) stateVar body
        eval me stateVar (returnPID pid)
      eval me stateVar (Free (Self returnMe)) = do
        --trace "self" $ return ()
        Internal {machine = (h,_)} <- liftIO $ readTVarIO stateVar
        let s = PID h me
        eval me stateVar $ returnMe s
      eval me stateVar (Free (Send you msg next)) = do
        --trace "send" $ return ()
        liftIO $ atomically $ do
          i@(Internal { machine = (h,sock)
                      , procs   = ps
                      , friends = fs
                      , fresh   = f
                      , mail    = m
                      }) <- readTVar stateVar
          writeTVar stateVar $
            i {mail = (PID h me,you, msg) <| m}
          return ()
        eval me stateVar next
      eval me stateVar (Free (Receive returnRest)) = (do
        mmsg <-
          liftIO $ atomically $ do
            i@(Internal { machine = (h,sock)
                        , procs   = ps
                        , friends = fs
                        , fresh   = f
                        , mail    = m
                        }) <- readTVar stateVar
            let (mmsg,rest) = find (PID h me) m
            writeTVar stateVar $ i {mail = rest}
            return mmsg
        case mmsg of
          Just msg -> eval me stateVar (returnRest msg)
          Nothing  -> do
            liftIO yield
            eval me stateVar (Free (Receive returnRest)))

          where
            find who Empty = (Nothing,Seq.empty)
            find who (rest :|> (from,to, msg))
              | to == who = (Just msg,rest)
              | otherwise = let (ans,rest') = find who rest
                            in (ans,rest |> (from,to,msg))

  in do
    state' <- liftIO state
    stateVar <- liftIO $ newTVarIO state'
    eval 0 stateVar actor
