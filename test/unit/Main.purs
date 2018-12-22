module Unit.Main where

import Prelude

import Data.Decimal (fromNumber, fromString) as Decimal
import Data.Map (fromFoldable)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (invalid, unV)
import Effect (Effect)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Tpay (validateResponse)
import Tpay as TPay
import Tpay.Request (defaultRequest)
import Tpay.Serialize (serialize)

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
    test "getUrl" $ do
      let
        amount = unsafePartial (fromJust $ Decimal.fromString "17.1")
        request = defaultRequest { id: "12", amount, description: "asdf something" }
        expectedUrl = "https://secure.tpay.com?accept_tos&address&amount=17.1&city&country&crc&custom_description&description=asdf%20something&email&expiration_date&group&id=12&language&md5sum=03e6e117f7fdf42e09c47bfc089ed829&merchant_description&name&online&phone&result_email&result_url&return_error_url&return_url&timehash&zip"
      url ← liftEffect $ TPay.getUrl { code: "", request }
      equal expectedUrl url
    test "validateResponse" $ do
      let
        response = "id=35866&tr_id=TR-PPV-10PYHYX&tr_date=2018-12-22+13%3A47%3A44&tr_crc=prod_0&tr_amount=9.50&tr_paid=9.50&tr_desc=desc&tr_status=TRUE&tr_error=none&tr_email=email%40example.com&test_mode=1&md5sum=3e0412eca6a2bf76b1a34a59396edba9"
        code = "wQxz8VifuN1cIh4F"
        expected = invalid []
      result <- liftEffect $ validateResponse { secret: code, response }
      unV
        (\err → failure (unsafeStringify err))
        (\r → do
            equal (r.trStatus) true
            equal (r.trAmount) (Decimal.fromNumber 9.5)
            equal (r.trPaid) (Decimal.fromNumber 9.5)
        )
        result

