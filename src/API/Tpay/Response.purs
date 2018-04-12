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


-- validators :: forall m. Monad m =>
--   { id :: Validator m (StrMap String) Int
--   , trId :: Validator m (StrMap String) String
--   , trAmount :: Validator m (StrMap String) Number
--   }

-- testVal = fromFoldable 
--   [ Tuple "id" "7"
--   , Tuple "tr_id" "SOMEID"
--   , Tuple "tr_amount" "42.42"
--   ]
-- 


-- -- test2 :: Identity _
-- -- test2 = recordCollect test1
-- 
-- -- instance validatorAlgebra ::
-- --   ( IsSymbol lbl
-- --   , RowCons lbl (m (V e b)) tail row
-- --   , RowLacks lbl tail
-- --   ) => Algebra "map" lbl (Validation m e a b) (Tuple a (Record tail)) (Tuple a (Record row)) where
-- --   algebra _ lbl val (Tuple a rec) =
-- --     let v' = unwrap val a
-- --     in insert lbl v' rec
