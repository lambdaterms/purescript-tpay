module API.Tpay.Response where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Data.Foldable (fold)
import Data.Record.Fold (collect)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.Crypto.Hash (Algorithm(..), hex)
import Polyform.Validation (Validation, hoistFnMV)
import Text.Parsing.StringParser (ParseError(..))
import Validators.Combinators (check)
import Validators.UrlEncoded (boolean, int, number, single, urlEncoded)

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

checkMd5 
  :: forall e
   . String 
  -> Validation
      (Eff (buffer :: BUFFER, crypto :: CRYPTO | e))
      (Array ParseError)
      (StrMap (Array String))
      (StrMap (Array String))
checkMd5 code = check msg $ lift2 (==) (single "md5sum") (str >>> calcMd5)
  where
    msg = (const [ParseError "Invalid md5"])
    str = ((_ <> code) <<< fold) <$> traverse single ["id", "tr_id", "tr_amount", "tr_crc"]
    calcMd5 = hoistFnMV $ \x -> pure <$> hex MD5 x

validateResponse
  :: forall e
  .  String
  -> Validation (Eff (buffer :: BUFFER, crypto :: CRYPTO | e)) (Array ParseError) String Response
validateResponse secret = 
      urlEncoded
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
