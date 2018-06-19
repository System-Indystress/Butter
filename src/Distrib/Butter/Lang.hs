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
import Control.Exception (catch, SomeException)
import Control.Monad.Free
import Network.Simple.TCP as N
import Control.Concurrent.Forkable
import Control.Concurrent (threadDelay)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..),(<|),(|>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BW
import Data.ByteString(ByteString(..))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as E
import Debug.Trace (trace, traceIO)
import Control.Monad (forever, guard)


data ProcessID = PID   {machineID :: Text, processID :: Int}
               | Named {hostname :: Text, processName :: Text}
  deriving (Show, Eq, Generic)

to :: Text -> Text -> ProcessID
to = Named

instance ToJSON (ProcessID) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON (ProcessID)


data Action (m :: * -> *) next where
    Lift    :: m a
            -> (a -> next)
            -> Action m next
    Connect :: Text
            -> Text
            -> Int
            -> next
            -> Action m next
    Spawn   :: Butter m ()
            -> (ProcessID -> next)
            -> Action m next
    Send    :: ProcessID
            -> LB.ByteString
            -> next
            -> Action m next
    Receive :: (LB.ByteString -> next)
            -> Action m next
    Friends :: ([Text] -> next)
            -> Action m next
    Self    :: (ProcessID -> next)
            -> Action m next
    Alive   :: ProcessID
            -> (Bool -> next)
            -> Action m next
    Name    :: Text
            -> next
            -> Action m next


data Internal =
  Internal { machine :: (Text)
           , procs   :: [(Int)]
           , names   :: Map Text Int
           , fresh   :: Int
           , friends :: Map Text Socket
           , mail    :: Seq (ProcessID, ProcessID, LB.ByteString)
           }

instance Functor (Action m) where
  fmap f (Spawn body returnPID)        = (Spawn body (f . returnPID))
  fmap f (Send pid msg next)           = (Send pid msg $ f next)
  fmap f (Receive returnMSG)           = (Receive $ f . returnMSG)
  fmap f (Connect name host port next) = (Connect name host port $ f next)
  fmap f (Friends returnFriends)       = (Friends $ f . returnFriends)
  fmap f (Lift ma returnA)             = (Lift ma $ f . returnA)
  fmap f (Self returnMe)               = (Self $ f . returnMe)
  fmap f (Alive pid returnB)           = (Alive pid $ f . returnB)
  fmap f (Name n next)                 = (Name n $ f next)

type Butter m a = Free (Action m) a

instance (MonadIO m) => MonadIO (Free (Action m)) where
  liftIO = lift . liftIO

connect :: Text -> Text -> Int -> Butter m ()
connect name host port = (Free (Connect name host port $ Pure ()))

spawn :: Butter m () -> Butter m ProcessID
spawn body = (Free (Spawn body (\i -> Pure  i)))

self ::  Butter m ProcessID
self = (Free (Self (\i -> Pure  i)))

name :: Text -> Butter m ()
name n = (Free (Name n $ Pure ()))

send :: (ToJSON a) => ProcessID -> a -> Butter m ()
send to msg = (Free (Send to (encode msg) $ Pure ()))

receive :: (MonadIO m, FromJSON a) => Butter m a
receive =
  (Free $ Receive $ \msg ->
            case decode msg of
              Nothing   -> do
                lift $ liftIO $ yield
                me <- self
                (Free $ Send me msg (Pure ()))
                receive
              Just a -> return a)

lift :: m a -> Butter m a
lift ma = Free (Lift ma $ \a -> Pure a)

alive :: ProcessID -> Butter m (Bool)
alive pid = (Free $ Alive pid $ \b -> Pure b)

spreadLocal :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a)
            => Butter m a -> m a
spreadLocal = spread "local" Nothing

spread :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a)
       => Text -> Maybe Int -> Butter m a -> m a
spread host mport actor =
  let
      getLength sock acc = do
        mbs <- recv sock 1
        case mbs of
          Nothing   -> getLength sock acc
          Just char ->
            if char == "§" then return acc else getLength sock $ acc `B.append` char
      setup :: (MonadIO m, ForkableMonad m) => TVar Internal -> Maybe Int -> m ()
      setup var Nothing     = return ()
      setup var (Just port) = do
        forkIO $ serve (HostAny) (show port) (\(sock,addr) -> forever $ do
          mbs <- recv sock 1
          if mbs /= Just (BW.pack "§")
          then return ()
          else do
              len <- getLength sock B.empty
              let len' = (read $ BW.unpack len) :: Int
              mbs' <- recv sock len'
              case mbs' of
                (Just payload) -> do
                  atomically $ modifyTVar var (\s ->
                    case (decode $ LB.fromStrict payload) of
                      (Just (f, Named _ n, msg)) ->
                        case M.lookup n (names s) of
                          Just i -> s{mail = (f, (PID (machine s) i), LB.pack msg)<| (mail s)}
                          Nothing -> s
                      (Just (f,t,msg)) ->
                        s{mail = (f, t, LB.pack msg)<| (mail s)}
                      _                -> s)
                  return ()
                _ -> return ())
        return ()

      eval :: (MonadIO m, ForkableMonad m, ToJSON a, FromJSON a) => Int -> TVar Internal -> Butter m a -> m a
      eval me stateVar (Pure a) = do
        liftIO $ atomically $ do
          i@(Internal { procs   = ps }) <- readTVar stateVar
          writeTVar stateVar $ i {procs = Prelude.filter (\n -> n /= me) ps}
        return a
      eval me stateVar (Free (Name n next)) = do
        liftIO $ atomically $ modifyTVar stateVar $ (\s ->
          s {names = M.insert n me (names s)})
        eval me stateVar next
      eval me stateVar (Free (Alive (PID _ n) returnB)) = do
        i@(Internal {procs = ps}) <- liftIO $ readTVarIO stateVar
        eval me stateVar $ returnB $ elem n ps
      eval me stateVar (Free (Alive (Named h n) returnB)) = do
        i <- liftIO $ readTVarIO stateVar
        if (machine i) == h
        then case M.lookup n $ names i of
               Just pnum -> eval me stateVar $ returnB $ elem pnum (procs i )
               Nothing   -> eval me stateVar $ returnB False
        else error "Unimplemented: Alive of remote processes"

      eval me stateVar (Free (Lift ma returnA)) = do
        a <- ma
        eval me stateVar $ returnA a
      eval me stateVar (Free (Friends returnFriends)) = do
        i@(Internal { machine = h
                    , procs   = ps
                    , friends = fs
                    , fresh   = f
                    , mail    = m
                    }) <- liftIO $ readTVarIO stateVar
        eval me stateVar $ returnFriends $ M.keys fs
      eval me stateVar (Free (Connect name host port rest)) = do
        let untilM f = do {x <- f; (case x of Nothing -> untilM f ; Just x -> return x)}
        (sock,addr) <- untilM $ liftIO $ (Just <$> connectSock (unpack host) (show port)) `catch` (\e -> let _ = e :: SomeException in return Nothing)
        liftIO $ atomically $ modifyTVar stateVar $ (\s ->
          s {friends = M.insert name sock $ friends s})
        eval me stateVar rest

      eval me stateVar (Free (Spawn body returnPID)) = do
        (h,f) <-
          liftIO $ atomically $ do
            i@(Internal { machine = h
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
        Internal {machine = h} <- liftIO $ readTVarIO stateVar
        let s = PID h me
        eval me stateVar $ returnMe s
      eval me stateVar (Free (Send you msg next)) = do
        --trace "send" $ return ()
        meffect <-
          liftIO $ atomically $ do
            i@(Internal { machine = h
                        , procs   = ps
                        , names   = n
                        , friends = fs
                        , fresh   = f
                        , mail    = m
                        }) <- readTVar stateVar
            case you of
               Named h' p | h == h' ->
                case M.lookup p n of
                  Nothing -> return Nothing
                  Just pnum -> do
                    writeTVar stateVar $
                      i {mail = (PID h me,PID h pnum, msg) <| m}
                    return Nothing
               Named h' p            ->
                 case M.lookup h' fs of
                   Nothing   -> return Nothing
                   Just sock ->  do
                     let payload = LB.toStrict $ encode $ (PID h me, Named h' p, LB.unpack msg)
                     return $ Just $ N.send sock (
                      "§"  `B.append` (BW.pack $ show $  (B.length payload)) `B.append` "§" `B.append` payload)
               pid -> do
                 writeTVar stateVar $ i {mail = (PID h me,pid, msg) <| m}
                 return Nothing
        case meffect of
          Just e -> liftIO e >> return ()
          Nothing -> return ()
        eval me stateVar next
      eval me stateVar (Free (Receive returnRest)) = (do
        mmsg <-
          liftIO $ atomically $ do
            i@(Internal { machine = h
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
    stateVar <- liftIO $ newTVarIO $
                   Internal { machine = host
                            , procs   = [0]
                            , names   = M.empty
                            , fresh   = 1
                            , friends = M.empty
                            , mail    = Seq.empty
                            }
    setup stateVar mport
    eval 0 stateVar actor
