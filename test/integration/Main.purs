module Integration.Main where

import Prelude

import API.Tpay.Request (prepareRequest, defaultRequest)
import API.Tpay.Response (validateResponse)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Polyform.Validation (V(..), runValidation)
import QuickServe (POST, RequestBody(..), quickServe)

server2 ∷ RequestBody String → POST String
server2 (RequestBody s) = do
  liftEffect $ log s
  val ← liftEffect $ runValidation (validateResponse "demo") s
  case val of
    Invalid e → liftEffect $ log "error"
    Valid e r → liftEffect $ log (show r)
  pure "TRUE"

main ∷ Effect Unit
main = do
  req ← prepareRequest "" $ defaultRequest { id: 12, amount: 17.1, description: "asdf" }
  log $ show req
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts server2
