module Tpay.Serialize
  ( class Serialize
  , class SerializeRecord
  , class SerializeValue
  , serialize
  , serializeImpl
  , serializeVal
  )
  where

import Prelude

import Data.Array as Array
import Data.Decimal (Decimal)
import Data.Decimal (toString) as Decimal
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record as Record
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class Serialize a where
  serialize ∷ a → Map String (Array String)

class SerializeRecord (list ∷ RowList) (rec ∷ # Type) | list → rec where
  serializeImpl ∷ RLProxy list → Record rec → Map String (Array String)

class SerializeValue a where
  serializeVal ∷ a → Array String

instance serializeRecord
    ∷ ( SerializeRecord proxy row , RowToList row proxy )
    ⇒ Serialize (Record row)
  where
  serialize = serializeImpl (RLProxy ∷ RLProxy proxy)

instance serializeCons
    ∷ 
    ( SerializeRecord proxy row
    , SerializeValue a
    , IsSymbol l
    , Cons l a rest row
    , Lacks l rest
    ) ⇒ SerializeRecord (Cons l a proxy) row
  where
  serializeImpl _ r =
    let
      key = (SProxy ∷ SProxy l)
      rest = serializeImpl (RLProxy ∷ RLProxy proxy) r
      val = serializeVal $ Record.get key r
    in
      Map.insert (reflectSymbol key) val rest

instance serializeNil ∷ SerializeRecord Nil a where
  serializeImpl _ _ = Map.empty

instance serializeValueArray
    ∷ 
    ( SerializeValue a
    , Foldable f
    ) ⇒ SerializeValue (f a)
  where
  serializeVal = Array.fromFoldable >=> serializeVal

instance serializeValueInt ∷ SerializeValue Int where
  serializeVal = show >>> pure

instance toStringNumber ∷ SerializeValue Number where
  serializeVal = show >>> pure

instance serializeValueString ∷ SerializeValue String where
  serializeVal = pure

instance serializeValueBoolean ∷ SerializeValue Boolean where
  serializeVal = show >>> pure

instance serializeValueDecimal ∷ SerializeValue Decimal where
  serializeVal = Decimal.toString >>> pure
