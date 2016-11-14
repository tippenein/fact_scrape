-- | Launch truth server.
module Main
  ( main
  ) where

import Protolude

import Truth.Server (startApp)

main :: IO ()
main = startApp
