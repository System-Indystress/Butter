{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Main where

import Distrib.Butter.Lang
import Distrib.Butter.Lib.Protocol
import Butter.HUnit

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Aeson
import GHC.Generics
data P = Reset
       | Val Int
       | Get
       | Add Int
       | Close
  deriving (Show, Eq, Generic)

instance FromJSON P
instance ToJSON P

instance Protocol P where
  data State P   = PS Int
  type Context P = IO
  setup Reset = return (PS 0)
  setup _     = error "Starting protocol P under non-initial conditions"
  handle Reset   _       = return $ NoReply (PS 0)
  handle (Val i) _       = return $ NoReply (PS i)
  handle Get     (PS i)  = return $ Reply (Val i) (PS i)
  handle (Add n) (PS m)  = return $ NoReply $ PS $ n + m
  handle Close   _       = return $ Terminate

main :: IO ()
main = do
  results <- runTestTT $ TestCase $
    spreadLocal $ do
      node <- start Reset
      b1   <- alive node
      (Val i1) <- call node Get
      lift $ threadDelay 1000
      cast node $ Val 10
      lift $ threadDelay 1000
      (Val i2) <- call node Get
      lift $ threadDelay 1000
      cast node $ Add 5
      lift $ threadDelay 1000
      (Val i3) <- call node Get
      lift $ threadDelay 1000
      cast node $ Add 4
      lift $ threadDelay 1000
      (Val i4) <- call node Get
      lift $ threadDelay 1000
      cast node Reset
      lift $ threadDelay 1000
      (Val i5) <- call node Get
      lift $ threadDelay 1000
      cast node Close
      lift $ threadDelay 1000
      b2       <- alive node
      lift $ assertEqual "node is alive" True  b1
      lift $ assertEqual "setup works"   0     i1
      lift $ assertEqual "cast val"      10    i2
      lift $ assertEqual "cast add"      15    i3
      lift $ assertEqual "cast add2"     19    i4
      lift $ assertEqual "cast reset"    0     i5
      lift $ assertEqual "node is dead"  False b2
      return ()
  checkFailure results

