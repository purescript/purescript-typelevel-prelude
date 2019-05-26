module Type.RowList
  ( module Prim.RowList
  , module RLProxy
  , class ListToRow
  , class RowListRemove
  , class RowListSet
  , class RowListNub
  , class RowListAppend
  ) where

import Prim.Row as Row
import Prim.RowList (kind RowList, Cons, Nil, class RowToList)
import Type.Equality (class TypeEquals)
import Type.Data.Symbol as Symbol
import Type.Data.Boolean as Boolean
import Type.Data.RowList (RLProxy)
import Type.Data.RowList (RLProxy(..)) as RLProxy

-- | Convert a RowList to a row of types.
-- | The inverse of this operation is `RowToList`.
class ListToRow (list :: RowList)
                (row :: # Type) |
                list -> row

instance listToRowNil
  :: ListToRow Nil ()

instance listToRowCons
  :: ( ListToRow tail tailRow
     , Row.Cons label ty tailRow row )
  => ListToRow (Cons label ty tail) row

-- | Remove all occurences of a given label from a RowList
class RowListRemove (label :: Symbol)
                    (input :: RowList)
                    (output :: RowList)
                    | label input -> output

instance rowListRemoveNil
  :: RowListRemove label Nil Nil

instance rowListRemoveCons
  :: ( RowListRemove label tail tailOutput
     , Symbol.Equals label key eq
     , Boolean.If eq
         (RLProxy tailOutput)
         (RLProxy (Cons key head tailOutput))
         (RLProxy output)
     )
  => RowListRemove label (Cons key head tail) output

-- | Add a label to a RowList after removing other occurences.
class RowListSet (label :: Symbol)
                 (typ :: Type)
                 (input :: RowList)
                 (output :: RowList)
                 | label typ input -> output

instance rowListSetImpl
  :: ( TypeEquals (Symbol.SProxy label) (Symbol.SProxy label')
     , TypeEquals typ typ'
     , RowListRemove label input lacking )
  => RowListSet label typ input (Cons label' typ' lacking)

-- | Remove label duplicates, keeps earlier occurrences.
class RowListNub (input :: RowList)
                 (output :: RowList)
                 | input -> output

instance rowListNubNil
  :: RowListNub Nil Nil

instance rowListNubCons
  :: ( TypeEquals (Symbol.SProxy label) (Symbol.SProxy label')
     , TypeEquals head head'
     , TypeEquals (RLProxy nubbed) (RLProxy nubbed')
     , RowListRemove label tail removed
     , RowListNub removed nubbed )
  => RowListNub (Cons label head tail) (Cons label' head' nubbed')

-- Append two row lists together
class RowListAppend (lhs :: RowList)
                    (rhs :: RowList)
                    (out :: RowList)
                    | lhs rhs -> out

instance rowListAppendNil
  :: TypeEquals (RLProxy rhs) (RLProxy out)
  => RowListAppend Nil rhs out

instance rowListAppendCons
  :: ( RowListAppend tail rhs out'
     , TypeEquals (RLProxy (Cons label head out')) (RLProxy out) )
  => RowListAppend (Cons label head tail) rhs out
