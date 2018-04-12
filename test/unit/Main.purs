module Unit.Main where

import Prelude

import API.Tpay.Serialize (serialize)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foreign (Foreign, ForeignError(..), MultipleErrors, readInt, toForeign)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Record (insert)
import Data.Record.Fold (applyTo, collect)
import Data.StrMap (StrMap, fromFoldable)
import Data.StrMap (fromFoldable)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (traceAnyA)
import Polyform.Input.Foreign (Attrs, IntField, NumberField, StringField, attr, object)
import Polyform.Validation (V, Validation(..), fromEither, runValidation)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Type.Row (class RowLacks)

data Json
  = Int (Array ForeignError)
  | Str (Array ForeignError)
  | Obj (Attrs Json)

v fv = Validation (pure <<< fromEither <<< lmap (Array.fromFoldable) <<< runExcept <<< fv)

obj = object Obj $ collect
  { field1: attr Int "field1" (v readInt)
  , field2: attr Int "field2" (v readInt)
  }

validators =
  { id: attr Int "id" (v readInt)
  -- , trId: attr "tr_id" (v readInt)
  -- -- , trAmount: selectField "tr_amount" >>> Validators.number
  , nested: attr Obj "nested" $ collect
      { field1: attr Int "field1" (v readInt)
      , field2: attr Int "field2" (v readInt)
      }
  }

test1 = object Obj $ collect validators

result1 = runValidation test1 (toForeign { id: 8, nested: { field1: "blabla" }})
result2 = runValidation test1 (toForeign { id: 8, nested: { field1: 8, field2: 9 }})

main
  :: forall eff
  . Eff
    ( timer :: TIMER
    , avar :: AVAR
    , console :: CONSOLE
    , testOutput :: TESTOUTPUT
    | eff
    )
    Unit
main = runTest $ do
  result1 >>= traceAnyA
  result2 >>= traceAnyA
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
