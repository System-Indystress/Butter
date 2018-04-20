{-#LANGUAGE KindSignatures, GADTs, DeriveGeneric, OverloadedStrings, TemplateHaskell #-}
module Distrib.Butter where

import Control.Monad.Trans.Except
import Data.Text
import Data.Aeson
import GHC.Generics
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import qualified Data.Map as M
import Data.Map (Map(..))
import Distrib.Type
import Control.Monad.Free
import Network.Simple.TCP
import Control.Concurrent.Forkable
import Debug.Trace (trace)

data ProcessID = PID {machineID :: Text, processID :: Int}
  deriving (Show, Eq, Generic)

instance ToJSON (ProcessID) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON (ProcessID)

data Process (m :: * -> *) next where
    Lift    :: m a
            -> (a -> next)
            -> Process m next
    Connect :: Text
            -> Int
            -> next
            -> Process m next
    Spawn   :: Actor m ()
            -> (ProcessID -> next)
            -> Process m next
    Send    :: (ToJSON a)
            => ProcessID
            -> a
            -> next
            -> Process m next
    Receive :: (FromJSON b)
            => (b -> next)
            -> Process m next
    Friends :: ([Text] -> next)
            -> Process m next
    Self    :: (ProcessID -> next)
            -> Process m next


data Internal =
  Internal { machine :: (Text,Socket)
           , procs   :: [(Int)]
           , fresh   :: Int
           , friends :: [(Text, Socket)]
           , mail    :: [(ProcessID, ProcessID, Value)]
           }

instance Functor (Process m) where
  fmap f (Spawn body returnPID)   = (Spawn body (f . returnPID))
  fmap f (Send pid msg next)      = (Send pid msg $ f next)
  fmap f (Receive returnMSG)      = (Receive $ f . returnMSG)
  fmap f (Connect host port next) = (Connect host port $ f next)
  fmap f (Friends returnFriends)  = (Friends $ f . returnFriends)
  fmap f (Lift ma returnA)        = (Lift ma $ f . returnA)
  fmap f (Self returnMe)          = (Self $ f . returnMe)

type Actor m a = Free (Process m) a


connect :: (Monad m) => Text -> Int -> Actor m ()
connect host port = (Free (Connect host port $ Pure ()))

spawn :: (Monad m) => Actor m () -> Actor m ProcessID
spawn body = (Free (Spawn body (\i -> Pure  i)))

self ::  (Monad m) => Actor m ProcessID
self = (Free (Self (\i -> Pure  i)))

send :: (ToJSON a, Monad m) => ProcessID -> a -> Actor m ()
send to msg = (Free (Send to msg $ Pure  ()))

receive :: (FromJSON a, Monad m) => Actor m a
receive = (Free $ Receive $ \msg -> Pure msg)

lift :: m a -> Actor m a
lift ma = Free (Lift ma $ \a -> Pure a)

friend :: Text -> ProcessID
friend host = PID host 0 

spreadLocal :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a)
            => Actor m a -> m a
spreadLocal = spread "local" 9099

spread :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a)
       => Text -> Int -> Actor m a -> m a
spread host port actor =
  let state :: (MonadIO m) => m Internal
      state = do
        (sock,_) <- liftIO $ bindSock HostAny (show port)
        return $
          Internal { machine = (host,sock)
                   , procs   = [0]
                   , fresh   = 1
                   , friends = []
                   , mail    = []
                   }
      mailman :: (MonadIO m) => TVar Internal -> m ()
      mailman stateVar = do
        Internal { machine = (h,sock)
                 , procs   = ps
                 , friends = fs
                 , fresh   = f
                 , mail    = m
                 } <- liftIO $ readTVarIO stateVar
        return ()

      eval :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a) => Int -> TVar Internal -> Actor m a -> m a
      eval me stateVar (Pure a) = return a
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
          let v = toJSON msg
          i@(Internal { machine = (h,sock)
                      , procs   = ps
                      , friends = fs
                      , fresh   = f
                      , mail    = m
                      }) <- readTVar stateVar
          writeTVar stateVar $
            i {mail = (PID h me,you, v) : m}
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
          Just msg -> do
            --trace (show msg) $ return ()
            let v = case fromJSON msg of
                      Error s   -> error $ "Receive error: " ++ s
                      Success a -> a
            eval me stateVar (returnRest v)
          Nothing  -> eval me stateVar (Free (Receive returnRest)))

          where
            find who [] = (Nothing,[])
            find who ((from,to, msg):rest)
              | to == who = (Just msg,rest)
              | otherwise = let (ans,rest') = find who rest
                            in (ans,(from,to,msg) : rest')

  in do
    state' <- liftIO state
    stateVar <- liftIO $ newTVarIO state'
    forkIO $ mailman stateVar
    eval 0 stateVar actor
