{-# LANGUAGE OverloadedStrings #-}
module Main where

import Distrib.Butter.Lang
import Test.HUnit
import Data.Text
import Control.Concurrent (forkIO)

butter1 :: Butter IO ()
butter1 = do
  me <- self
  send me (1 :: Int)
  y <- receive :: Butter IO Int
  lift $ assertEqual "send receive self Int msg" 1  y
  return ()

butter2 :: Butter IO ()
butter2 = do
  me <- self
  child <- spawn $ do
    x <- receive :: Butter IO Int
    send me (x+1)
    return ()
  send child (5 :: Int)
  y <- receive :: Butter IO Int
  lift $ assertEqual "spawn, send receive Int msg" 6 y
  return ()


selective :: Butter IO ()
selective = do
  me <- self
  spawn $ do
    send me ("hello" :: String)
    send me True
  b <- receive :: Butter IO Bool
  s <- receive :: Butter IO String
  lift $ assertEqual "receive out of order by type" True b
  lift $ assertEqual "receive out of order by type" "hello" s
  return ()

distr :: IO ()
distr = do
  forkIO $ spread "0.0.0.0" (Just 8000) $ do
    name "hello"
    msg <- receive :: Butter IO String
    lift $ putStrLn msg
    lift $ assertEqual "got the message from another node" "*wave*" msg
    return ()
  forkIO $ spread "0.0.0.0" (Just 8001) $ do
    connect "friend" "0.0.0.0" 8000
    send (to "friend" "hello") ("*wave*" :: String)
    return ()
  return ()

distr2 :: IO ()
distr2 = do
  forkIO $ spread "0.0.0.0" (Just 8002) $ do
    name "ping"
    connect "node2" "0.0.0.0" 8003
    send (to "node2" "hello") ("ping" :: Text)
    msg <- receive :: Butter IO Text
    lift $ assertEqual "got message" "pong" msg
    return ()
  forkIO $ spread "0.0.0.0" (Just 8003) $ do
    name "hello"
    connect "node1" "0.0.0.0" 8002
    who <- receive :: Butter IO Text
    send (to "node1" who) ("pong" :: Text)
    return ()
  return ()





main :: IO ()
main = do
  runTestTT $
    TestList [ TestLabel "self,send,receive"       $
               TestCase $ spreadLocal butter1
             , TestLabel "self,send,receive,spawn" $
               TestCase $ spreadLocal butter2
             , TestLabel "receive out of order by type" $
               TestCase $ spreadLocal selective
             , TestLabel "two nodes communicating one way" $
               TestCase distr
             , TestLabel "sending an name over and replying" $
               TestCase distr2
             ]
  return ()
