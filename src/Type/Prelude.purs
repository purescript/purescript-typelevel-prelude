module Type.Prelude
  ( module Equality
  , module Data.Ordering
  , module Proxy
  , module Data.Symbol
  ) where

import Type.Equality (class TypeEquals, from, to) as Equality
import Type.Data.Ordering (kind Ordering, LT, EQ, GT, OProxy(..), class IsOrdering, reflectOrdering, reifyOrdering) as Data.Ordering
import Type.Proxy (Proxy(..)) as Proxy
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol, reifySymbol) as Data.Symbol
