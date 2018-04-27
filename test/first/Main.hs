module Main where

import Distrib.Butter.Lang
import Test.HUnit

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

main :: IO ()
main = do
  runTestTT $
    TestList [ TestLabel "self,send,receive"       $
               TestCase $ spreadLocal butter1
             , TestLabel "self,send,receive,spawn" $
               TestCase $ spreadLocal butter2
             ]
  return ()
