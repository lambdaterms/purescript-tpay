module API.Tpay.Request where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Record as Record
import Data.Symbol (SProxy(..))
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.Crypto.Hash as Hash

type RequestBase r =
  { id :: Int
  , amount :: Number
  , description :: String
  | r
  }

type Request = RequestBase ()

type RequestInternal = RequestBase ( md5sum :: String )

prepareRequest :: Request -> Eff (buffer :: BUFFER, crypto :: CRYPTO) RequestInternal
prepareRequest r = let
    str = show r.id <> show r.amount <> show r.description
  in do
    md5 <- Hash.hex Hash.MD5 str
    let r' = Record.insert (SProxy :: SProxy "md5sum") md5 r
    pure $ r'
