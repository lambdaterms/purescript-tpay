module API.Tpay.Validators
  ( Validator
  , boolean
  , int
  , md5
  , number
  , response
  , selectField
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (fold)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
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
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.Crypto.Hash as Hash
import Polyform.Validation (V(..), Validation(..), runValidation)
import Polyform.Validation as V
import Text.Parsing.StringParser (ParseError(..), runParser)

type Validator m a b = Validation m (Array ParseError) a b

parseError :: forall a. String -> V (Array ParseError) a
parseError s = Invalid $ Array.singleton $ ParseError s

fromEither :: forall a. Either ParseError a -> V (Array ParseError) a
fromEither = lmap Array.singleton >>> V.fromEither

response :: forall m. Monad m => Validator m String (StrMap String)
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


number :: forall m. Monad m => Validator m String Number
number = Validation \s -> pure $ case Number.fromString s of
  Just n -> pure n
  Nothing -> parseError $ "Could not parse " <> s <> " as number"

int :: forall m. Monad m => Validator m String Int
int = Validation \s -> pure $ case Int.fromString s of
  Just n -> pure n
  Nothing -> parseError $ "Could not parse " <> s <> " as int"

boolean :: forall m. Monad m => Validator m String Boolean
boolean = Validation \s -> pure $ case String.toLower s of
  "false" -> pure false
  "true" -> pure true
  _ -> parseError $ "Could not parse " <> s <> " as boolean"

selectField :: forall m. Monad m => String -> Validator m (StrMap String) String
selectField f = Validation \q -> pure $ case StrMap.lookup f q of
  Just s -> pure s
  Nothing -> parseError $ "Could not find field " <> f

type Secret = String

md5 :: forall e
   . String
  -> Secret
  -> Validator (Eff (buffer :: BUFFER, crypto :: CRYPTO | e)) (StrMap String) (StrMap String)
md5 lbl code = (Validation (\m -> map (map (Tuple m)) (runValidation (selectField lbl) m)))
  >>> Validation \(Tuple m md5sum) -> do
    let vals = fold $ (\key -> Array.fromFoldable (StrMap.lookup key m)) <$> ["id", "tr_id", "tr_amount", "tr_crc"]
    computedMd5 <- Hash.hex Hash.MD5 (fold vals <> code)
    pure (if computedMd5 == md5sum then pure m else parseError $ "md5 sums do not match")
