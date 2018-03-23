module API.Tpay.Response where

import Prelude

import API.Tpay.Validators (Validator, selectField)
import API.Tpay.Validators as Validators
import Control.Monad.Eff (Eff)
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
  Validators.response >>> Validators.md5 "md5sum" secret
  >>> (builder
  <$> (selectField "id" >>> Validators.int)
  <*> selectField "tr_id"
  <*> selectField "tr_date"
  <*> selectField "tr_crc"
  <*> (selectField "tr_amount" >>> Validators.number)
  <*> (selectField "tr_paid" >>> Validators.number)
  <*> selectField "tr_desc"
  <*> (selectField "tr_status" >>> Validators.boolean)
  <*> selectField "tr_error"
  <*> selectField "tr_email")
  where
    builder =
      { id: _
      , trId: _
      , trDate: _
      , trCrc: _
      , trAmount: _
      , trPaid: _
      , trDesc: _
      , trStatus: _
      , trError: _
      , trEmail: _
      }
