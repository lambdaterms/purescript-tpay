module API.Tpay.Request where

import Prelude

import API.Tpay.Serialize (serialize, serializeVal)
import Control.Monad.Eff (Eff)
import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.Crypto.Hash as Hash

type RequestBase r =
  ( id :: Int
  , amount :: Number
  , description :: String
  | r
  )

type RequestOptional r =
  ( crc :: Maybe String
  | r
  )

type Request = Record (RequestBase (RequestOptional ()))
type RequestInternal = Record (RequestBase (RequestOptional (md5sum :: String )))

prepareRequest
  :: forall e
  .  Request
  -> Eff (buffer :: BUFFER, crypto :: CRYPTO | e) (StrMap (Array String))
prepareRequest (r@{ id, amount, description, crc }) =
  let
    strs :: Array String
    strs = serializeVal id <> serializeVal amount <> serializeVal crc
    str :: String 
    str = fold strs
  in do
    md5 <- Hash.hex Hash.MD5 str
    pure $ StrMap.insert "md5sum" [md5] (serialize r)
