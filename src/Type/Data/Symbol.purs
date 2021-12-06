module Type.Data.Symbol
  ( append
  , class Equals
  , class Reverse
  , class ReverseP
  , class Snoc
  , compare
  , equals
  , reverse
  , module Data.Symbol
  , module Prim.Symbol
  , snoc
  , uncons
  )
  where

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


-- Reverse

class ReverseP (a :: Symbol) (reversed :: Symbol) | a -> reversed, reversed -> a

instance reverseEmpty :: ReverseP "" ""
else
instance reverseCons :: 
  ( Cons head tail sym
  , ReverseP tailReverse tail
  , ReverseP tail tailReverse
  , Append tailReverse head reversed
  ) => ReverseP sym reversed

class Reverse (a :: Symbol) (reversed :: Symbol) | a -> reversed, reversed -> a

instance reverseReverseP :: (ReverseP a b, ReverseP b a) => Reverse a b

--| ```purescript
--| > :t reverse (Proxy :: Proxy "symbol")
--| Proxy @Symbol "lobmys"
--| ```
-- | 
reverse :: ∀ proxy a b. Reverse a b => proxy a -> Proxy b
reverse _ = Proxy

-- Snoc

--| ```purescript
--| Snoc "symbo" "l" ?x ~~> ?x = "symbol"
--| Snoc ?a ?b "symbol" ~~> ?a = "symbo", ?b = "l"
--| ```
-- | `end` must be a single character
class Snoc (list :: Symbol) (end :: Symbol) (symbol :: Symbol) | end list -> symbol, symbol -> end list

instance snocReverse :: (Reverse sym reversed, Cons end listReverse reversed, Reverse listReverse list) => Snoc list end sym

--| ```purescript
--| > :t snoc (Proxy :: Proxy "symbo") (Proxy :: Proxy "l")
--| Proxy @Symbol "symbol"
--| ```
snoc :: ∀a b c. Snoc a b c => Proxy a -> Proxy b -> Proxy c
snoc _ _ = Proxy
