{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Data.Text (Text)
import Database
import Servant.API

truthApi :: Proxy TruthApi
truthApi = Proxy

type TruthApi =
       ListPersons
  :<|> ListStatements

type ListPersons = "persons" :> Get '[JSON] [Person]

type ListStatements =
     "statements"
  :> QueryParam "person_name" Text
  :> Get '[JSON] [PersonStatement]
