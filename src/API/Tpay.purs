module API.Tpay where

import Prelude

import API.Tpay.Request (Request, prepareRequest)
import Data.Array as Array
import Data.FormURLEncoded (encode)
import Data.FormURLEncoded as FormURLEncoded
import Data.Map as Map
import Effect (Effect)

url ∷ String
url = "https://secure.tpay.com"

get ∷ String → Request → Effect String
get code req = do
  body ← postBody code req
  pure $ url <> "?" <> body

postBody ∷ String → Request → Effect String
postBody code req = do
  obj ← prepareRequest code req
  pure $ obj
    # map Array.head
    # Map.toUnfoldable
    # FormURLEncoded.fromArray
    # encode
