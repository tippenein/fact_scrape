{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Data.Text (Text)
import Database
import Servant.API

data PersonTruthiness = PersonTruthiness 
  { truthValue :: String
  , total :: Integer
  }

truthApi :: Proxy TruthApi
truthApi = Proxy

type TruthApi =
       ListPersons
  :<|> ListStatements
  :<|> ListTruthiness

type ListPersons = "persons" :> Get '[JSON] [Person]
type ListTruthiness = 
     "persons" 
  :> Capture "person_name"
  :> "truthiness" 
  :> Get '[JSON] [(String, Int)]

type ListStatements =
     "statements"
  :> QueryParam "person_name" Text
  :> Get '[JSON] [PersonStatement]
