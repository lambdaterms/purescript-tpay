module API.Tpay.Response where

import Prelude

import Data.Foreign (F, Foreign, ForeignError(..), fail, readInt, readNumber, readString)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Index ((!))

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

newtype ResponseInternal = ResI (ResponseBase (md5sum :: String))

instance decodeResponseInternal :: Decode ResponseInternal where
  decode = decodeResponse

decodeResponse :: Foreign -> F ResponseInternal
decodeResponse val = do
  id <- val ! "id" >>= readInt
  trId <- val ! "tr_id" >>= readString
  trDate <- val ! "tr_date" >>= readString
  trCrc <- val ! "tr_crc" >>= readString
  trAmount <- val ! "tr_amount" >>= readNumber
  trPaid <- val ! "tr_paid" >>= readNumber
  trDesc <- val ! "tr_desc" >>= readString
  trStatus <- val ! "tr_status" >>= readString >>= case _ of
    "TRUE" -> pure true
    "FALSE" -> pure false
    _ -> fail $ ForeignError "expected either TRUE or FALSE"
  trError <- val ! "tr_error" >>= readString
  trEmail <- val ! "tr_email" >>= readString
  md5sum <- val ! "md5sum" >>= readString
  pure $ ResI { id, trId, trDate, trCrc, trAmount, trPaid, trDesc, trStatus, trError, trEmail, md5sum }
