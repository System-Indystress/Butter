{-#LANGUAGE KindSignatures, GADTs, DeriveGeneric, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Distrib.Butter.Lib.K where

import Distrib.Butter.Lang
import Data.Aeson
import GHC.Generics
import Control.Monad.Free

deriving instance Generic a => Generic (Free f a)

class (Monad m) => SerialM m where
  toV   :: m a -> Value
  fromV :: Value -> m a

sendB :: (SerialM m, ToJSON a) => ProcessID -> Butter m a -> Butter m ()
sendB to program = undefined

receiveB :: (SerialM m, FromJSON a) => Butter m (Butter m a)
receiveB = undefined
