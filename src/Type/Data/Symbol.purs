module Type.Data.Symbol
  ( append
  , class Equals
  , class Mirror
  , class MirrorP
  , class Snoc
  , compare
  , equals
  , mirror
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


-- Mirror

class MirrorP (a :: Symbol) (mirror :: Symbol) | a -> mirror, mirror -> a

instance mirrorEmpty :: MirrorP "" ""
else
instance mirrorCons :: 
  ( Cons head tail sym
  , MirrorP tailMirror tail
  , MirrorP tail tailMirror
  , Append tailMirror head mirror
  ) => MirrorP sym mirror

class Mirror (a :: Symbol) (mirror :: Symbol) | a -> mirror, mirror -> a

instance mirrorMirrorP :: (MirrorP a b, MirrorP b a) => Mirror a b

mirror :: ∀ proxy a b. Mirror a b => proxy a -> Proxy b
mirror _ = Proxy

-- Snoc

--| ```purescript
--| Snoc "symbo" "l" ?x ~~> ?x = "symbol"
--| Snoc ?a ?b "symbol" ~~> ?a = "symbo", ?b = "l"
--| ```
-- | `end` must be a single character
class Snoc (list :: Symbol) (end :: Symbol) (symbol :: Symbol) | end list -> symbol, symbol -> end list

instance snocMirror :: (Mirror sym mirror, Cons end listMirror mirror, Mirror listMirror list) => Snoc list end sym

--| ```purescript
--| snoc (Proxy :: Proxy "symbo") (Proxy :: Proxy "l") = Proxy :: Proxy "symbol"
--| ```
snoc :: ∀a b c. Snoc a b c => Proxy a -> Proxy b -> Proxy c
snoc _ _ = Proxy
