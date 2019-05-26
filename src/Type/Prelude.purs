module Type.Prelude
  ( module Type.Data.Boolean
  , module Type.Data.Ordering
  , module Type.Data.Symbol
  , module Type.Equality
  , module Type.Proxy
  , module Type.Row
  , module Type.RowList
  ) where

import Type.Data.Boolean (kind Boolean, True, False, BProxy(..), class IsBoolean, reflectBoolean, reifyBoolean)
import Type.Data.Ordering (kind Ordering, LT, EQ, GT, OProxy(..), class IsOrdering, reflectOrdering, reifyOrdering)
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol, reifySymbol, class Compare, compare, class Append, append)
import Type.Equality (class TypeEquals, from, to)
import Type.Row (class Union, class Lacks, RProxy(..))
import Type.RowList (class RowToList, class ListToRow, RLProxy(..))
