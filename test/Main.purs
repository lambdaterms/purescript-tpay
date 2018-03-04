module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP)
import QuickServe (GET, POST, RequestBody(..), quickServe)

server2 :: forall r. RequestBody String -> POST (console :: CONSOLE | r) String
server2 (RequestBody s) = do
  liftEff $ log s
  pure "TRUE"

server :: forall r. GET r String
server = pure "Hello, World!"


main :: forall eff . Eff (console :: CONSOLE, http :: HTTP | eff) Unit
main = do
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts server2
