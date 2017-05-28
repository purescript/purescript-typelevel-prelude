module Type.Row
  ( class RowLacks
  , class RowLacking
  ) where

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

