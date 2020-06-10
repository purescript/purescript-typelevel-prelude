module Type.Data.Symbol
  ( module Prim.Symbol
  , module Data.Symbol
  , append
  , compare
  , uncons
  , class Equals
  , class Mirror
  , class MirrorP
  , mirror
  , class Snoc
  , snoc
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


-- Mirror

class MirrorP (a :: Symbol) (mirror :: Symbol) | a -> mirror, mirror -> a

instance mirrorEmpty :: MirrorP "" ""
else
instance mirrorCons :: (Cons head tail sym, MirrorP tailMirror tail, MirrorP tail tailMirror, Append tailMirror head mirror) => MirrorP sym mirror

class Mirror (a :: Symbol) (mirror :: Symbol) | a -> mirror, mirror -> a

instance mirrorMirrorP :: (MirrorP a b, MirrorP b a) => Mirror a b

mirror :: ∀a b. Mirror a b => SProxy a -> SProxy b
mirror _ = SProxy

-- Snoc

--| ```purescript
--| Snoc "symbo" "l" ?x ~~> ?x = "symbol"
--| Snoc ?a ?b "symbol" ~~> ?a = "symbo", ?b = "l"
--| ```
-- | `end` must be a single character
class Snoc (list :: Symbol) (end :: Symbol) (symbol :: Symbol) | end list -> symbol, symbol -> end list

instance snocMirror :: (Mirror sym mirror, Cons end listMirror mirror, Mirror listMirror list) => Snoc list end sym

--| ```purescript
--| snoc (SProxy :: SProxy "symbo") (SProxy :: SProxy "l") = SProxy :: SProxy "symbol"
--| ```
snoc :: ∀a b c. Snoc a b c => SProxy a -> SProxy b -> SProxy c
snoc _ _ = SProxy