module Main where

import Distrib.Butter
import Test.HUnit

actor1 :: Actor IO ()
actor1 = do
  me <- self
  send me (1 :: Int)
  y <- receive :: Actor IO Int
  lift $ assertEqual "send receive self Int msg" 1  y
  return ()

actor2 :: Actor IO ()
actor2 = do
  me <- self
  child <- spawn $ do
    x <- receive :: Actor IO Int
    send me (x+1)
    return ()
  send child (5 :: Int)
  y <- receive :: Actor IO Int
  lift $ assertEqual "spawn, send receive Int msg" 6 y
  return ()

main :: IO ()
main = do
  runTestTT $
    TestList [ TestLabel "self,send,receive"       $
               TestCase $ spreadLocal actor1
             , TestLabel "self,send,receive,spawn" $
               TestCase $ spreadLocal actor2
             ]
  return ()
