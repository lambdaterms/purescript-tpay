module API.Tpay.Serialize where

import Prelude

import Data.Array as Array
import Data.List (List(..), foldl)
import Data.Maybe (Maybe(..))
import Data.Record as Record
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

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
  ) => SerializeValue (Array a) (Array b) where
  serializeVal = map serializeVal

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

serialize
  :: forall row list b
  .  RowToList row list
  => SerializeRecord list row b
  => Record row -> StrMap b
serialize = serializeImpl (RLProxy :: RLProxy list)

convertToPostArray :: StrMap (Array String) -> Array (Tuple String (Maybe String))
convertToPostArray m = 
  let list = StrMap.fold (\a k vals -> foldl (\a v -> Cons (Tuple k (Just v)) a) a vals) Nil m
  in Array.fromFoldable list
