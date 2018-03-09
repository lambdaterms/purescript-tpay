module API.Tpay.Response where

import Prelude

import API.Tpay.Validators (Validator, selectField)
import API.Tpay.Validators as Validators

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

type ResponseInternal = ResponseBase (md5sum :: String)

validateResponse :: Validator String { id :: Int, trAmount :: Number, trDesc :: String }
validateResponse = Validators.response
  >>> ({ id: _, trAmount: _, trDesc: _ }
  <$> (selectField "id" >>> Validators.int)
  <*> (selectField "tr_amount" >>> Validators.number)
  <*> selectField "tr_desc")

-- buildResponse =
--   { id: _
--   , trId: _
--   , trDate: _
--   , trCrc: _
--   , trAmount: _
--   , trPaid: _
--   , trDesc: _
--   , trStatus: _
--   , trError: _
--   , trEmail: _
--   , md5sum: _
--   }
