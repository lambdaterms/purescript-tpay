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


class Serialize a where
  serialize :: a -> StrMap (Array String)

instance serializeRecord ::
  ( SerializeRecord proxy row
  , RowToList row proxy
  ) => Serialize (Record row) where
  serialize = serializeImpl (RLProxy :: RLProxy proxy)


class SerializeRecord (list :: RowList) (rec :: # Type) | list -> rec where
  serializeImpl :: RLProxy list -> Record rec -> StrMap (Array String)

instance serializeCons :: 
  ( SerializeRecord proxy row
  , SerializeValue a
  , IsSymbol l
  , RowCons l a rest row
  , RowLacks l rest
  ) => SerializeRecord (Cons l a proxy) row where
  serializeImpl _ r =
    let
      key = (SProxy :: SProxy l)
      rest = serializeImpl (RLProxy :: RLProxy proxy) r
      val = serializeVal $ Record.get key r
      r'  = StrMap.insert (reflectSymbol key) val rest
    in r'

instance serializeNil :: SerializeRecord Nil a where
  serializeImpl _ _ = StrMap.empty


class SerializeValue a where
  serializeVal :: a -> Array String

instance serializeValueArray :: 
  ( SerializeValue a
  , Foldable f
  ) => SerializeValue (f a) where
  serializeVal = Array.fromFoldable >=> serializeVal

instance serializeValueInt :: SerializeValue Int where
  serializeVal = show >>> pure

instance toStringNumber :: SerializeValue Number where
  serializeVal = show >>> pure

instance serializeValueString :: SerializeValue String where
  serializeVal = pure

instance serializeValueBoolean :: SerializeValue Boolean where
  serializeVal = show >>> pure
