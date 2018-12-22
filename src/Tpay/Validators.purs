module Tpay.Validators where

import Prelude

import Data.Decimal (Decimal)
import Data.Decimal (fromString) as Decimal
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (invalid)
import Polyform.Validator (hoistFn, hoistFnV)
import Polyform.Validators.UrlEncoded (FieldValueValidator, string)

decimal :: forall m. Monad m => FieldValueValidator m Decimal
decimal = flip compose string $ hoistFnV $ \s -> case Decimal.fromString s of
  Just n -> pure n
  Nothing -> invalid $ "Could not parse " <> s <> " as number"

boolean :: forall m. Monad m => FieldValueValidator m Boolean
boolean = flip compose string $ hoistFn $ case _ of
  "TRUE" -> true
  otherwise -> false
