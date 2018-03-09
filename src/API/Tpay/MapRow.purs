module API.Tpay.MapRow where

import Prelude

import Data.Record as Record
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class MapShowRow (list :: RowList) (from :: # Type) | list -> from where
  mapShowRow :: RLProxy list -> Record from -> StrMap String

class ToString a where
  toString :: a -> String

instance toStringInt :: ToString Int where
  toString = show

instance toStringNumber :: ToString Number where
  toString = show

instance toStringString :: ToString String where
  toString x = x 

instance mapShowRowCons :: 
  ( MapShowRow xs rowRest
  , ToString a
  , IsSymbol l
  , RowCons l a rowRest row
  , RowLacks l rowRest
  , RowToList row (Cons l a xs)
  , RowToList rowRest xs
  ) => MapShowRow (Cons l a xs) row where
  mapShowRow _ r =
    let
      sym = (SProxy :: SProxy l)
      rest = mapShowRow (RLProxy :: RLProxy xs) $ Record.delete sym r
      val = toString $ Record.get sym r
      r'  = StrMap.insert (reflectSymbol sym) val rest
    in r'

instance mapShowRowNil :: MapShowRow Nil () where
  mapShowRow _ _ = StrMap.empty

mapShow
  :: forall row list
  . RowToList row list
  => MapShowRow list row
  => Record row -> StrMap String
mapShow = mapShowRow (RLProxy :: RLProxy list)
