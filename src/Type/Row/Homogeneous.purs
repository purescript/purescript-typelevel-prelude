module Type.Row.Homogeneous
  ( class FieldOf
  , class Homogeneous
  ) where

-- | Ensure that every field in a row has the same type.
import Type.Equality (class TypeEquals)
import Type.Row (class RowToList, Cons, Nil, kind RowList)

-- | Ensure that every field in a row has the same type.
class Homogeneous (row :: # Type) fieldType | row -> fieldType
instance homogeneous
  :: ( RowToList row fields
     , FieldOf fields fieldType )
  => Homogeneous row fieldType

class FieldOf (rowList :: RowList) fieldType | rowList -> fieldType
instance fieldOfCons
  :: ( FieldOf tail fieldType
     , TypeEquals fieldType fieldType2 )
  => FieldOf (Cons symbol fieldType tail) fieldType2
instance fieldOfNil :: FieldOf Nil fieldType
