module Integration.Main where

import Prelude

import API.Tpay.Request (prepareRequest, defaultRequest)
import API.Tpay.Response (validateResponse)
import Data.Decimal (fromString) as Decimal
import Data.Maybe (Maybe(..), fromJust)
import Data.Validation.Semigroup (unV)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Polyform.Validator (runValidator)
import QuickServe (RequestBody(..), POST, quickServe)

server2 :: RequestBody String -> POST String
server2 (RequestBody s) = do
  log s
  val <- liftEffect $ runValidator (validateResponse "demo") s
  unV (log <<< unsafeStringify) (log <<< unsafeStringify) val
  pure "TRUE"

main ::  Effect Unit
main = do
  let
    amount = unsafePartial (fromJust $ Decimal.fromString "17.1")
  req ← prepareRequest "" $ defaultRequest { id: "12", amount, description: "asdf" }
  log $ show req
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts server2
