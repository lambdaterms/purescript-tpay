module API.Tpay.Response where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (fold)
import Data.Record.Fold (collect)
import Data.Traversable (traverse)
import Data.Variant (inj)
import Effect (Effect)
import Node.Crypto.Hash (Algorithm(..), hex)
import Polyform.Validator (hoistFnMV)
import Polyform.Validators (Validator, check)
import Polyform.Validators.UrlEncoded (Error) as Urlencoded
import Polyform.Validators.UrlEncoded (boolean, field, int, number, parse, string)
import Polyform.Validators.UrlEncoded.Types (Decoded) as Urlencoded.Types
import Type.Prelude (SProxy(..))
import Type.Row (type (+))


type ResponseError err = Urlencoded.Error + (md5 ∷ String | err)

type ResponseBase r =
  { id ∷ Int
  , trId ∷ String
  , trDate ∷ String
  , trCrc ∷ String
  , trAmount ∷ Number
  , trPaid ∷ Number
  , trDesc ∷ String
  , trStatus ∷ Boolean
  , trError ∷ String
  , trEmail ∷ String
  | r
  }

type Response = ResponseBase ()
type ResponseInternal = ResponseBase (md5sum ∷ String)

checkMd5
  ∷ ∀ e
   . String
  -> Validator
      Effect
      (ResponseError + e)
      Urlencoded.Types.Decoded
      Urlencoded.Types.Decoded
checkMd5 code = check msg $ lift2 (==) (field "md5sum" string) (str >>> calcMd5)
  where
    msg = (const [inj (SProxy ∷ SProxy "md5") "Invalid md5"])
    str = ((_ <> code) <<< fold) <$> traverse (flip field string) ["id", "tr_id", "tr_amount", "tr_crc"]
    calcMd5 = hoistFnMV $ \x → pure <$> hex MD5 x

validateResponse
  :: forall e
  .  String
  -> Validator Effect (ResponseError + e) String Response
validateResponse secret
  = parse { replacePlus: true }
  >>> checkMd5 secret
  >>> validators
  where
    validators = collect
      { id: field "id" int
      , trId: field "tr_id" string
      , trDate: field "tr_date" string
      , trCrc: field "tr_crc" string
      , trAmount: field "tr_amount" number
      , trPaid: field "tr_paid" number
      , trDesc: field "tr_desc" string
      , trStatus: field "tr_status" boolean
      , trError: field "tr_error" string
      , trEmail: field "tr_email" string
      }
