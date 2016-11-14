{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for truth.
module Truth.API
  ( API
  , api
  , PersonTruthiness(..)
  ) where

import Protolude

import Data.Aeson
import Servant.API

import Data.Text (Text)
import Truth.Server.Database
import Database.Persist

data PersonTruthiness = PersonTruthiness
  { tVal  :: Text
  , total :: Int
  } deriving (Show, ToJSON, Generic, Eq)

-- | Value-level representation of API.
api :: Proxy API
api = Proxy

type API =
       ListPersons
  :<|> FindPerson
  :<|> ListStatements
  :<|> ListTruthiness

type ListPersons =
     "persons"
  :> QueryParam "q" Text
  :> Get '[JSON] [Entity Person]

type FindPerson =
     "persons"
  :> Capture "person_id" Int64
  :> Get '[JSON] (Entity Person)

type ListStatements =
     "statements"
  :> QueryParam "person_name" Text
  :> Get '[JSON] [PersonStatement]

type ListTruthiness =
     "persons"
  :> Capture "person_id" Int64
  :> "truthiness"
  :> Get '[JSON] [PersonTruthiness]
