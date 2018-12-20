module Tpay where

import Prelude

import Tpay.Request (Request, prepareRequest)
import Data.Array as Array
import Data.FormURLEncoded (encode)
import Data.FormURLEncoded as FormURLEncoded
import Data.Map as Map
import Effect (Effect)

type Url = String

url ∷ { code ∷ String, request ∷  Request } → Effect Url
url { code, request } = do
  body ← query
  pure $ "https://secure.tpay.com?" <> body
  where
    query ∷ Effect String
    query = do
      obj ← prepareRequest code request
      pure $ obj
        # map Array.head
        # Map.toUnfoldable
        # FormURLEncoded.fromArray
        # encode
