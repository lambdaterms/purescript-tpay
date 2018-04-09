module API.Tpay.Response where

import Prelude

import API.Tpay.Validators (Validator, selectField)
import API.Tpay.Validators as Validators
import Control.Monad.Eff (Eff)
import Data.Record.Fold (collect)
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)

type ResponseBase r =
  { id :: Int
  , trId :: String
  , trDate :: String
  , trCrc :: String
  , trAmount :: Number
  , trPaid :: Number
  , trDesc :: String
  , trStatus :: Boolean
  , trError :: String
  , trEmail :: String
  | r
  }

type Response = ResponseBase ()
type ResponseInternal = ResponseBase (md5sum :: String)

validateResponse
  :: forall e
  .  String
  -> Validator (Eff (buffer :: BUFFER, crypto :: CRYPTO | e)) String Response
validateResponse secret = 
      Validators.response 
  >>> Validators.md5 "md5sum" secret
  >>> validators
  where
    validators = collect
      { id: selectField "id" >>> Validators.int
      , trId: selectField "tr_id"
      , trDate: selectField "tr_date"
      , trCrc: selectField "tr_crc"
      , trAmount: selectField "tr_amount" >>> Validators.number
      , trPaid: selectField "tr_paid" >>> Validators.number
      , trDesc: selectField "tr_desc"
      , trStatus: selectField "tr_status" >>> Validators.boolean
      , trError: selectField "tr_error"
      , trEmail: selectField "tr_email"
      }
