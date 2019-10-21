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
import Type.Data.Ordering (OProxy(..), EQ)
import Type.Data.Ordering (class Equals) as Ordering
import Type.Data.Boolean (kind Boolean, BProxy(..))

compare :: forall l r o. Compare l r o => SProxy l -> SProxy r -> OProxy o
compare _ _ = OProxy

append :: forall l r o. Append l r o => SProxy l -> SProxy r -> SProxy o
append _ _ = SProxy

uncons :: forall h t s. Cons h t s => SProxy s -> {head :: SProxy h, tail :: SProxy t}
uncons _ = {head : SProxy, tail : SProxy}

class Equals (lhs :: Symbol)
             (rhs :: Symbol)
             (out :: Boolean) |
             lhs rhs -> out

instance equalsSymbol
  :: (Compare lhs rhs ord,
      Ordering.Equals EQ ord out)
  => Equals lhs rhs out

equals :: forall l r o. Equals l r o => SProxy l -> SProxy r -> BProxy o
equals _ _ = BProxy

