module API.Tpay.Serialize 
  ( class Serialize
  , class SerializeRecord
  , class SerializeValue
  , serialize
  , serializeImpl
  , serializeVal
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Record as Record
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)


class Serialize a b where
  serialize :: a -> StrMap b

instance serializeRecord ::
  ( SerializeRecord proxy row b
  , RowToList row proxy
  ) => Serialize (Record row) b where
  serialize = serializeImpl (RLProxy :: RLProxy proxy)


class SerializeRecord (list :: RowList) (rec :: # Type) b | list -> rec where
  serializeImpl :: RLProxy list -> Record rec -> StrMap b

instance serializeCons :: 
  ( SerializeRecord proxy row b
  , SerializeValue a b
  , IsSymbol l
  , RowCons l a rest row
  , RowLacks l rest
  ) => SerializeRecord (Cons l a proxy) row b where
  serializeImpl _ r =
    let
      key = (SProxy :: SProxy l)
      rest = serializeImpl (RLProxy :: RLProxy proxy) r
      val = serializeVal $ Record.get key r
      r'  = StrMap.insert (reflectSymbol key) val rest
    in r'

instance serializeNil :: SerializeRecord Nil a b where
  serializeImpl _ _ = StrMap.empty


class SerializeValue a b where
  serializeVal :: a -> b

instance serializeValueArray :: 
  ( SerializeValue a b
  , Foldable f
  ) => SerializeValue (f a) (Array b) where
  serializeVal = Array.fromFoldable >>> map serializeVal

instance serializeValueInj ::
  ( SerializeValue a b
  ) => SerializeValue a (Array b) where
  serializeVal = serializeVal >>> pure

instance serializeValueInt :: SerializeValue Int String where
  serializeVal = show

instance toStringNumber :: SerializeValue Number String where
  serializeVal = show

instance serializeValueString :: SerializeValue String String where
  serializeVal x = x
