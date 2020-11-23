module Type.Data.Symbol
  ( module Prim.Symbol
  , module Data.Symbol
  , append
  , compare
  , uncons
  , class Equals
  , equals
  ) where

import Prim.Symbol (class Append, class Compare, class Cons)
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol, reifySymbol)
import Type.Data.Ordering (EQ)
import Type.Data.Ordering (class Equals) as Ordering
import Type.Proxy (Proxy(..))

compare :: forall proxy l r o. Compare l r o => proxy l -> proxy r -> Proxy o
compare _ _ = Proxy

append :: forall proxy l r o. Append l r o => proxy l -> proxy r -> Proxy o
append _ _ = Proxy

uncons :: forall proxy h t s. Cons h t s => proxy s -> {head :: Proxy h, tail :: Proxy t}
uncons _ = {head : Proxy, tail : Proxy}

class Equals :: Symbol -> Symbol -> Boolean -> Constraint
class Equals lhs rhs out | lhs rhs -> out

instance equalsSymbol
  :: (Compare lhs rhs ord,
      Ordering.Equals EQ ord out)
  => Equals lhs rhs out

equals :: forall proxy l r o. Equals l r o => proxy l -> proxy r -> Proxy o
equals _ _ = Proxy
