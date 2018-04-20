{-#LANGUAGE DeriveGeneric #-}
module Distrib.Type (Type) where
import Data.Aeson
import GHC.Generics

import Language.Haskell.TH.Syntax


instance ToJSON Type
instance FromJSON Type

instance ToJSON TyLit
instance FromJSON TyLit

instance ToJSON TyVarBndr
instance FromJSON TyVarBndr

instance ToJSON Name
instance FromJSON Name

instance ToJSON OccName
instance FromJSON OccName

instance ToJSON NameFlavour
instance FromJSON NameFlavour

instance ToJSON PkgName
instance FromJSON PkgName

instance ToJSON NameSpace
instance FromJSON NameSpace

instance ToJSON ModName
instance FromJSON ModName
