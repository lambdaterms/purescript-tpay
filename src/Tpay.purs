module Tpay where

import Prelude

import Tpay.Request (Request, prepareRequest)
import Data.Array as Array
import Data.FormURLEncoded (encode)
import Data.FormURLEncoded as FormURLEncoded
import Data.Map as Map
import Effect (Effect)

-- | Excerpt from tpay documentation:
-- | Simple integration of the merchant service with tpay.com is a redirection of the customer
-- | to https://secure.tpay.com, along with the parameters required to define the transaction.
-- | These parameters must be sent by POST or GET. The POST method is preferred.

url ∷ String
url = "https://secure.tpay.com"

type Url = String

getUrl ∷ { code ∷ String, request ∷  Request } → Effect Url
getUrl r = do
  body ← postBody r
  pure $ url <> "?" <> body

type FormURLEncodedString = String

postBody ∷ { code ∷ String, request ∷ Request } → Effect FormURLEncodedString
postBody { code, request } = do
  obj ← prepareRequest code request
  pure $ obj
    # map Array.head
    # Map.toUnfoldable
    # FormURLEncoded.fromArray
    # encode
