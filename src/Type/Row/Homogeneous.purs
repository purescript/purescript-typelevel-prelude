module Type.Row.Homogeneous
  ( class Homogeneous
  , class HomogenousRowList
  ) where

import Type.Equality (class TypeEquals)
import Type.Row (class RowToList, Cons, Nil, kind RowList)

-- | Ensure that every field in a row has the same type.
class Homogeneous (row :: # Type) fieldType | row -> fieldType
instance homogeneous
  :: ( RowToList row fields
     , HomogenousRowList fields fieldType )
  => Homogeneous row fieldType

class HomogenousRowList (rowList :: RowList) fieldType | rowList -> fieldType
instance homogenousRowListCons
  :: ( HomogenousRowList tail fieldType
     , TypeEquals fieldType fieldType2 )
  => HomogenousRowList (Cons symbol fieldType tail) fieldType2
instance homogenousRowListNil :: HomogenousRowList Nil fieldType
