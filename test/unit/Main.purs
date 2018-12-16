module Unit.Main where

import Prelude

import API.Tpay.Serialize (serialize)
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest $ do
  suite "API.TPay.MapRow" $ do
    test "serializes simple record with simple types" $ do
      let
        expected =
          fromFoldable
            [ (Tuple "test1"  ["15"])
            , (Tuple "id" ["12"])
            , (Tuple "description" ["asdf"])
            , (Tuple "amount" ["17.1"])
            ]
        query = serialize { id: 12, amount: 17.1, description: "asdf", test1: 15 }
      equal expected query
