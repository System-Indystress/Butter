module Butter.HUnit
  ( module Test.HUnit
  , checkFailure
  ) where
import Test.HUnit
import System.Exit (exitWith, ExitCode(..), exitSuccess)

checkFailure results
  | errors results + failures results == 0 = exitSuccess
  | otherwise = exitWith (ExitFailure 1)

