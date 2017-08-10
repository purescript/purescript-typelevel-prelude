module Type.Data.Record
  ( class RecordOf
  ) where

import Type.Row (class RowToList, Cons, Nil, kind RowList)

class RecordOf record fieldType
instance recordOf
  :: ( RowToList record fields
     , FieldOf fields fieldType
     )
  => RecordOf (Record record) fieldType

class FieldOf (rowList :: RowList) fieldType
instance fieldOfCons
  :: (FieldOf tail fieldType)
  => FieldOf (Cons symbol fieldType tail) fieldType
instance fieldOfNil :: FieldOf Nil fieldType
