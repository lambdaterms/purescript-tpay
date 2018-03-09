module API.Tpay.Request where

import Prelude

import API.Tpay.MapRow (class MapShowRow, mapShow)
import Control.Monad.Eff (Eff)
import Data.Foldable (fold)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.Crypto.Hash as Hash
import Type.Row (class RowToList)

type RequestBase r =
  ( id :: Int
  , amount :: Number
  , description :: String
  | r
  )

type RequestMax = RequestBase (test1 :: Int, test2 :: String)
type RequestMin = RequestBase ()

class RowSubset (s :: # Type) (t :: # Type)

instance rowSubset :: (Union r t s) => RowSubset r s

type Request = RequestBase ()
type RequestInternal = Record (RequestBase ( md5sum :: String ))

class IsProperRequest (r :: # Type)

instance isProperRequest :: (RowSubset r RequestMax, RowSubset RequestMin r) => IsProperRequest r

prepareRequest
  :: forall r ls e
  .  IsProperRequest r
  => MapShowRow ls r
  => RowToList r ls
  => Record r -> Eff (buffer :: BUFFER, crypto :: CRYPTO | e) (StrMap String)
prepareRequest r = 
  let
    vals = mapShow r
    str  = fold <<< StrMap.values $ vals
  in do
    md5 <- Hash.hex Hash.MD5 str
    pure $ StrMap.insert "md5sum" md5 vals
