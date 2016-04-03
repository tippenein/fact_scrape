{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Api where

import GHC.Generics
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Database
import Servant.API

data PersonTruthiness = PersonTruthiness
  { tVal :: Text
  , total :: Int
  } deriving (Show, ToJSON, Generic, Eq)

truthApi :: Proxy TruthApi
truthApi = Proxy

type TruthApi =
       ListPersons
  :<|> ListStatements
  :<|> ListTruthiness

type ListPersons = "persons" :> Get '[JSON] [Person]

type ListStatements =
     "statements"
  :> QueryParam "person_name" Text
  :> Get '[JSON] [PersonStatement]

type ListTruthiness =
     "persons"
  :> "truthiness"
  :> QueryParam "person_name" Text
  :> Get '[JSON] [(Person, PersonTruthiness)]

