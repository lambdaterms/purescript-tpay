module API.Tpay.Validators
  ( Validator
  , boolean
  , int
  , number
  , response
  , selectField
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.URI (Query(..))
import Data.URI.Query as Query
import Polyform.Validation (V(..), Validation(..))
import Polyform.Validation as V
import Text.Parsing.StringParser (ParseError(..), runParser)

type Validator a b = Validation Identity (Array ParseError) a b

parseError :: forall a. String -> V (Array ParseError) a
parseError s = Invalid $ Array.singleton $ ParseError s

fromEither :: forall a. Either ParseError a -> V (Array ParseError) a
fromEither = lmap Array.singleton >>> V.fromEither

response :: Validator String (StrMap String)
response = Validation \s ->
  pure (queryToMap <$> (fromEither $ runParser Query.parser ("?" <> s)))

queryToMap :: Query -> StrMap String
queryToMap (Query q) =
  let
    q' = do
      Tuple k v <- q
      case v of
        Just s -> pure $ Tuple k s
        Nothing -> Nil
  in
    StrMap.fromFoldable q'


number :: Validator String Number
number = Validation \s -> pure $ case Number.fromString s of
  Just n -> pure n
  Nothing -> parseError $ "Could not parse " <> s <> " as number"

int :: Validator String Int
int = Validation \s -> pure $ case Int.fromString s of
  Just n -> pure n
  Nothing -> parseError $ "Could not parse " <> s <> " as int"

boolean :: Validator String Boolean
boolean = Validation \s -> pure $ case String.toLower s of
  "false" -> pure false
  "true" -> pure true
  _ -> parseError $ "Could not parse " <> s <> " as boolean"

selectField :: String -> Validator (StrMap String) String
selectField f = Validation \q -> pure $ case StrMap.lookup f q of
  Just s -> pure s
  Nothing -> parseError $ "Could not find field " <> f
