module Type.Data.Ordering
  ( module PO
  , OProxy(..)
  , class IsOrdering
  , reflectOrdering
  , reifyOrdering
  , class Append
  , append
  , class Invert
  , invert
  , class Equals
  , class Compare
  , compare
  , class IsLt
  , isLt
  , class IsEq
  , isEq
  , class IsGt
  , isGt
  , class IsLte
  , isLte
  , class IsGte
  , isGte
  ) where

import Prim.Ordering (LT, EQ, GT, Ordering) as PO
import Type.Data.Boolean (True, False)
import Type.Proxy (Proxy(..))
import Data.Ordering (Ordering(..))
import Data.Symbol (SProxy)
import Prim.Symbol (class Compare) as Symbol
import Prim.Boolean(True, False)
import Type.Data.Boolean (class Or, BProxy(..))

-- | Value proxy for `Ordering` types
-- | **Deprecated:** Use `Type.Proxy` instead
data OProxy :: PO.Ordering -> Type
data OProxy ordering = OProxy

-- | Class for reflecting a type level `Ordering` at the value level
class IsOrdering :: PO.Ordering -> Constraint
class IsOrdering ordering where
  reflectOrdering :: forall proxy. proxy ordering -> Ordering

instance isOrderingLT :: IsOrdering PO.LT where reflectOrdering _ = LT
instance isOrderingEQ :: IsOrdering PO.EQ where reflectOrdering _ = EQ
instance isOrderingGT :: IsOrdering PO.GT where reflectOrdering _ = GT

-- | Use a value level `Ordering` as a type-level `Ordering`
reifyOrdering :: forall r. Ordering -> (forall proxy o. IsOrdering o => proxy o -> r) -> r
reifyOrdering LT f = f (Proxy :: Proxy PO.LT)
reifyOrdering EQ f = f (Proxy :: Proxy PO.EQ)
reifyOrdering GT f = f (Proxy :: Proxy PO.GT)

-- | Append two `Ordering` types together
-- | Reflective of the semigroup for value level `Ordering`
class Append :: PO.Ordering -> PO.Ordering -> PO.Ordering -> Constraint
class Append lhs rhs output | lhs -> rhs output
instance appendOrderingLT :: Append PO.LT rhs PO.LT
instance appendOrderingEQ :: Append PO.EQ rhs rhs
instance appendOrderingGT :: Append PO.GT rhs PO.GT

append :: forall proxy l r o. Append l r o => proxy l -> proxy r -> Proxy o
append _ _ = Proxy

-- | Invert an `Ordering`
class Invert :: PO.Ordering -> PO.Ordering -> Constraint
class Invert ordering result | ordering -> result
instance invertOrderingLT :: Invert PO.LT PO.GT
instance invertOrderingEQ :: Invert PO.EQ PO.EQ
instance invertOrderingGT :: Invert PO.GT PO.LT

invert :: forall proxy i o. Invert i o => proxy i -> Proxy o
invert _ = Proxy

class Equals :: PO.Ordering -> PO.Ordering -> Boolean -> Constraint
class Equals lhs rhs out | lhs rhs -> out

instance equalsEQEQ :: Equals PO.EQ PO.EQ True
instance equalsLTLT :: Equals PO.LT PO.LT True
instance equalsGTGT :: Equals PO.GT PO.GT True
instance equalsEQLT :: Equals PO.EQ PO.LT False
instance equalsEQGT :: Equals PO.EQ PO.GT False
instance equalsLTEQ :: Equals PO.LT PO.EQ False
instance equalsLTGT :: Equals PO.LT PO.GT False
instance equalsGTLT :: Equals PO.GT PO.LT False
instance equalsGTEQ :: Equals PO.GT PO.EQ False

-- | Compares type a b
class Compare :: forall k. k -> k -> Ordering -> Constraint
class Compare a b (o :: Ordering) | a b -> o

compare :: forall a b o. Compare a b o => a -> b -> OProxy o
compare _ _ = OProxy

class IsLt :: forall k. k -> k -> Boolean -> Constraint
class IsLt a b (isLt :: Boolean) | a b -> isLt
instance isLtTrue ∷ (Compare a b o, Equals o PO.LT isLt) => IsLt a b isLt

isLt :: forall a b isLt. IsLt a b isLt => a -> b -> BProxy isLt
isLt _ _ = BProxy

class IsGt :: forall k. k -> k -> Boolean -> Constraint
class IsGt a b (isGt :: Boolean) | a b -> isGt
instance isGtCompare :: (Compare a b o, Equals o PO.GT isGt) => IsGt a b isGt

isGt :: forall a b isGt. IsGt a b isGt => a -> b -> BProxy isGt
isGt _ _ = BProxy

class IsEq :: forall k. k -> k -> Boolean -> Constraint
class IsEq a b (isEq :: Boolean) | a b -> isEq
instance isEqCompare :: (Compare a b o, Equals o PO.EQ isEq) => IsEq a b isEq

isEq :: forall a b isEq. IsEq a b isEq => a -> b -> BProxy isEq
isEq _ _ = BProxy

class IsLte :: forall k. k -> k -> Boolean -> Constraint
class IsLte a b (isLte :: Boolean) | a b -> isLte
instance isLteFromIs ∷ (IsEq a b isEq, IsLt a b isLt, Or isEq isLt isLte) => IsLte a b isLte

isLte :: forall a b isLte. IsLte a b isLte => a -> b -> BProxy isLte
isLte _ _ = BProxy

class IsGte :: forall k. k -> k -> Boolean -> Constraint
class IsGte a b (isGte :: Boolean) | a b -> isGte
instance isGteFromIs :: (IsEq a b isEq, IsGt a b isGt, Or isEq isGt isGte) => IsGte a b isGte

isGte :: forall a b isGte. IsGte a b isGte => a -> b -> BProxy isGte
isGte _ _ = BProxy

instance compareOrd :: Symbol.Compare lhs rhs ord => Compare (SProxy lhs) (SProxy rhs) ord
