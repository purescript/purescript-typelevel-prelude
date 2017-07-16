module Type.Row
  ( RProxy(..)
  , class RowLacks
  , class RowLacking
  , kind RowList
  , Nil
  , Cons
  , RLProxy(..)
  , class RowToList
  , class ListToRow
  , class RowListRemove
  , class RowListSet
  , class RowListNub
  , class RowListAppend
  ) where

import Type.Equality (class TypeEquals)
import Type.Data.Symbol as Symbol
import Type.Data.Boolean as Boolean

data RProxy (row :: # Type) = RProxy

-- Must not be exported
foreign import data Entry :: Type

-- | If you get "No type class instance was found" for this class, then your
-- | `row` shouldn't contain the label `key`.
class RowLacking (entry :: Type)
                 (key :: Symbol)
                 (typ :: Type)
                 (row :: # Type) |
                 entry typ -> key row

instance rowLacking :: RowLacking entry key entry row

-- | Encodes the constraint that a given row does not contain a specific key.
class RowLacks (key :: Symbol)
               (row :: # Type)

-- Append `Entry` at label `key` to the right of `row` then lookup `key` on the
-- left - if we check via instance solving that the `typ` we get back is
-- `Entry`, then `row` lacks `key`.  In the case that `row` doesn't lack
-- `key`, we get a "No type class instance found" error for:
-- `RowLacking Entry key typ row`.
instance rowLacks
  :: ( RowCons key Entry () keyEntry
     , Union row keyEntry rowKeyEntry
     , RowCons key typ ignored rowKeyEntry
     , RowLacking Entry key typ row )
  => RowLacks key row


-- | A type-level list representation of a row
foreign import kind RowList
foreign import data Nil :: RowList
foreign import data Cons :: Symbol -> Type -> RowList -> RowList

data RLProxy (rowList :: RowList) = RLProxy

-- | Extract the collection of entries in a closed row of types.
-- | The list of entries is sorted by label and preserves duplicates.
-- | The inverse of this operation is `ListToRow`.
-- | Solved by the compiler.
class RowToList (row :: # Type)
                (list :: RowList) |
                row -> list

-- | Convert a RowList to a row of types.
-- | The inverse of this operation is `RowToList`.
class ListToRow (list :: RowList)
                (row :: # Type) |
                list -> row

instance listToRowNil
  :: ListToRow Nil ()

instance listToRowCons
  :: ( ListToRow tail tailRow
     , RowCons label ty tailRow row )
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

