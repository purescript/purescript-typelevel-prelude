module Type.Prelude
  ( module Type.Data.Boolean
  , module Type.Data.Ordering
  , module Type.Data.Symbol
  , module Type.Equality
  , module Type.Proxy
  , module Type.Row
  , module Type.RowList
  ) where

import Type.Data.Boolean (class IsBoolean, False, True, reflectBoolean, reifyBoolean)
import Type.Data.Ordering (class IsOrdering, EQ, GT, LT, Ordering, reflectOrdering, reifyOrdering)
import Type.Data.Symbol (class Append, class Compare, class IsSymbol, append, compare, reflectSymbol, reifySymbol)
import Type.Equality (class TypeEquals, from, to)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Union)
import Type.RowList (class ListToRow, class RowToList)
