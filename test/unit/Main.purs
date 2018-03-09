module Unit.Main where

import Prelude

import API.Tpay.MapRow (mapShow)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main :: forall eff. Eff ( timer :: TIMER
                        , avar :: AVAR
                        , console :: CONSOLE
                        , testOutput :: TESTOUTPUT | eff ) Unit
main = runTest $ do
  suite "API.TPay.MapRow" $ do
    test "serializes simple record with simple types" $ do
      let
        expected =
          fromFoldable
            [ (Tuple "test1" "15")
            , (Tuple "id" "12")
            , (Tuple "description" "asdf")
            , (Tuple "amount" "17.1")
            ]
        query = mapShow { id: 12, amount: 17.1, description: "asdf", test1: 15 }
      equal expected query
