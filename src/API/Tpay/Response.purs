module API.Tpay.Response where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (fold)
import Data.Record.Fold (collect)
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Foreign.Object (Object)
import Node.Crypto.Hash (Algorithm(..), hex)
import Polyform.Validator (Validator, hoistFnMV)
import Polyform.Validators (check)
import Polyform.Validators.UrlEncoded (boolean, int, number, single, urlEncoded)
import Type.Prelude (SProxy(..))


type ResponseError err = Variant (urlError ∷ String, md5 ∷ String | err)

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
  ∷ ∀ err
   . String
  -> Validator
      Effect
      (Array (ResponseError err))
      (Object (Array String))
      (Object (Array String))
checkMd5 code = check msg $ lift2 (==) (single "md5sum") (str >>> calcMd5)
  where
    msg = (const [inj (SProxy ∷ SProxy "md5") "Invalid md5"])
    str = ((_ <> code) <<< fold) <$> traverse single ["id", "tr_id", "tr_amount", "tr_crc"]
    calcMd5 = hoistFnMV $ \x → pure <$> hex MD5 x

validateResponse
  :: forall err
  .  String
  -> Validator Effect (Array (ResponseError err)) String Response
validateResponse secret
  = urlEncoded { replacePlus: true }
  >>> checkMd5 secret
  >>> validators
  where
    validators = collect
      { id: single "id" >>> int
      , trId: single "tr_id"
      , trDate: single "tr_date"
      , trCrc: single "tr_crc"
      , trAmount: single "tr_amount" >>> number
      , trPaid: single "tr_paid" >>> number
      , trDesc: single "tr_desc"
      , trStatus: single "tr_status" >>> boolean
      , trError: single "tr_error"
      , trEmail: single "tr_email"
      }
