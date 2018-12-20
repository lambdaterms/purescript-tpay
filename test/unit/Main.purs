module Unit.Main where

import Prelude

import Tpay as TPay
import Tpay.Request (defaultRequest)
import Tpay.Serialize (serialize)
import Data.Decimal (fromString) as Decimal
import Data.Map (fromFoldable)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest $ do
  suite "TPay.Serialize" $ do
    test "serializes simple record with simple types" $ do
      let
        expected =
          fromFoldable
            [ (Tuple "test1"  ["15"])
            , (Tuple "id" ["12"])
            , (Tuple "description" ["asdf"])
            , (Tuple "amount" ["17.1"])
            ]
        query = serialize { id: "12", amount: 17.1, description: "asdf", test1: 15 }
      equal expected query
  suite "TPay" $ do
    let
      amount = unsafePartial (fromJust $ Decimal.fromString "17.1")
      request = defaultRequest { id: "12", amount, description: "asdf" }
      expectedUrl = "https://secure.tpay.com?accept_tos&address&amount=17.1&city&country&crc&custom_description&description=asdf&email&expiration_date&group&id=12&language&md5sum=03e6e117f7fdf42e09c47bfc089ed829&merchant_description&name&online&phone&result_email&result_url&return_error_url&return_url&timehash&zip"
    test "simple post body" $ do
      url ← liftEffect $ TPay.getUrl { code: "", request }
      equal expectedUrl url
