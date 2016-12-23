module Type.Prelude
  ( module Type.Equality
  , module Type.Data.Ordering
  , module Type.Proxy
  , module Data.Symbol
  ) where

import Type.Equality (class TypeEquals, from, to)
import Type.Data.Ordering (kind Ordering, LT, EQ, GT, OProxy(..), class IsOrdering, reflectOrdering, reifyOrdering)
import Type.Proxy (Proxy(..))
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol, reifySymbol)
