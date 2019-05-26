module Type.Row
  ( module Prim.Row
  , module RProxy
  , RowApply
  , type (+)
  ) where

import Prim.Row (class Lacks, class Nub, class Cons, class Union)
import Type.Data.Row (RProxy(..)) as RProxy

-- | Type application for rows.
type RowApply (f :: # Type -> # Type) (a :: # Type) = f a

-- | Applies a type alias of open rows to a set of rows. The primary use case
-- | this operator is as convenient sugar for combining open rows without
-- | parentheses.
-- | ```purescript
-- | type Rows1 r = (a :: Int, b :: String | r)
-- | type Rows2 r = (c :: Boolean | r)
-- | type Rows3 r = (Rows1 + Rows2 + r)
-- | type Rows4 r = (d :: String | Rows1 + Rows2 + r)
-- | ```
infixr 0 type RowApply as +
