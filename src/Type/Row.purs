module Type.Row
  ( module Prim.Row
  , module RProxy
  , RowApply
  , type (+)
  ) where

import Prim.Row (class Lacks, class Nub, class Cons, class Union)
import Type.Data.Row (RProxy(..)) as RProxy

-- | Polymorphic Type application
-- |
-- | For example...
-- | ```
-- | APPLY Maybe Int == Maybe $ Int == Maybe Int
-- | ```
type APPLY ∷ ∀ d c . (d → c) → d → c
type APPLY f a = f a

infixr 0 type APPLY as $

-- | Reversed polymorphic Type application
-- |
-- | For example...
-- | ```
-- | FLIP Int Maybe == Maybe Int
-- | ```
-- | Note: an infix for FLIP (e.g. `Int # Maybe`) is not allowed.
-- | Before the `0.14.0` release, we used `# Type` to refer to a row of types.
-- | In  the `0.14.0` release, the `# Type` syntax was deprecated,
-- | and `Row Type` is the correct way to do this now. To help mitigate
-- | breakage, `# Type` was made an alias to `Row Type`. When the `# Type`
-- | syntax is fully dropped in a later language release, we can then
-- | support the infix version: `Int # Maybe`.
type FLIP ∷ ∀ d c . d → (d → c) → c
type FLIP a f = f a

-- | Type application for rows.
type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a

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
