module Tpay
  ( module Tpay.Request
  , module Tpay.Response
  , getUrl
  , postBody
  , url
  )where

import Prelude

import Data.Array as Array
import Data.FormURLEncoded (encode)
import Data.FormURLEncoded as FormURLEncoded
import Data.Map as Map
import Effect (Effect)
import Tpay.Request (defaultRequest, Request, prepareRequest) as Tpay.Request
import Tpay.Response (Response, validateResponse) as Tpay.Response

-- | Excerpt from tpay documentation:
-- | Simple integration of the merchant service with tpay.com is a redirection of the customer
-- | to https://secure.tpay.com, along with the parameters required to define the transaction.
-- | These parameters must be sent by POST or GET. The POST method is preferred.

url ∷ String
url = "https://secure.tpay.com"

type Url = String

getUrl ∷ { code ∷ String, request ∷  Tpay.Request.Request } → Effect Url
getUrl r = do
  body ← postBody r
  pure $ url <> "?" <> body

type FormURLEncodedString = String

postBody ∷ { code ∷ String, request ∷ Tpay.Request.Request } → Effect FormURLEncodedString
postBody { code, request } = do
  obj ← Tpay.Request.prepareRequest code request
  pure $ obj
    # map Array.head
    # Map.toUnfoldable
    # FormURLEncoded.fromArray
    # encode
