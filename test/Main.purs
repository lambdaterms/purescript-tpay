module Test.Main where

import Prelude

import API.Tpay.Request (prepareRequest)
import API.Tpay.Response (validateResponse)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Record.ShowRecord (showRecord)
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.HTTP (HTTP)
import Polyform.Validation (V(..), runValidation)
import QuickServe (GET, POST, RequestBody(..), quickServe)
 
server2 :: forall r. RequestBody String -> POST (console :: CONSOLE | r) String
server2 (RequestBody s) = do
  liftEff $ log s
  let val = unwrap $ runValidation validateResponse s
  case val of
    Invalid e -> liftEff $ log (show e)
    Valid e r -> liftEff $ log (showRecord r)
  pure "TRUE"

server :: forall r. GET r String
server = pure "Hello, World!"

main :: forall eff . Eff (console :: CONSOLE, http :: HTTP, buffer :: BUFFER, crypto :: CRYPTO | eff) Unit
main = do
  req <- prepareRequest { id: 12, amount: 17.1, description: "asdf", test1: 15 }
  log $ show req
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts server2
