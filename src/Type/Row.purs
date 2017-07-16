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
  ) where

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
