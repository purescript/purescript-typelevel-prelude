module Type.Data.Ordering
  ( module Prim.Ordering
  , OProxy(..)
  , class IsOrdering
  , reflectOrdering
  , reifyOrdering
  , class Append
  , append
  , class Invert
  , invert
  , class Equals
  , equals
  ) where

import Prim.Ordering (kind Ordering, LT, EQ, GT)
import Data.Ordering (Ordering(..))
import Type.Data.Boolean (kind Boolean, True, False, BProxy(..))

-- | Value proxy for `Ordering` types
data OProxy (ordering :: Ordering) = OProxy

-- | Class for reflecting a type level `Ordering` at the value level
class IsOrdering (ordering :: Ordering) where
  reflectOrdering :: OProxy ordering -> Ordering

instance isOrderingLT :: IsOrdering LT where reflectOrdering _ = LT
instance isOrderingEQ :: IsOrdering EQ where reflectOrdering _ = EQ
instance isOrderingGT :: IsOrdering GT where reflectOrdering _ = GT

-- | Use a value level `Ordering` as a type-level `Ordering`
reifyOrdering :: forall r. Ordering -> (forall o. IsOrdering o => OProxy o -> r) -> r
reifyOrdering LT f = f (OProxy :: OProxy LT)
reifyOrdering EQ f = f (OProxy :: OProxy EQ)
reifyOrdering GT f = f (OProxy :: OProxy GT)

-- | Append two `Ordering` types together
-- | Reflective of the semigroup for value level `Ordering`
class Append (lhs :: Ordering)
             (rhs :: Ordering)
             (output :: Ordering) |
             lhs -> rhs output
instance appendOrderingLT :: Append LT rhs LT
instance appendOrderingEQ :: Append EQ rhs rhs
instance appendOrderingGT :: Append GT rhs GT

append :: forall l r o. Append l r o => OProxy l -> OProxy r -> OProxy o
append _ _ = OProxy

-- | Invert an `Ordering`
class Invert (ordering :: Ordering)
             (result :: Ordering) |
             ordering -> result
instance invertOrderingLT :: Invert LT GT
instance invertOrderingEQ :: Invert EQ EQ
instance invertOrderingGT :: Invert GT LT

invert :: forall i o. Invert i o => OProxy i -> OProxy o
invert _ = OProxy

class Equals (lhs :: Ordering)
             (rhs :: Ordering)
             (out :: Boolean) |
             lhs rhs -> out

instance equalsEQEQ :: Equals EQ EQ True
instance equalsLTLT :: Equals LT LT True
instance equalsGTGT :: Equals GT GT True
instance equalsEQLT :: Equals EQ LT False
instance equalsEQGT :: Equals EQ GT False
instance equalsLTEQ :: Equals LT EQ False
instance equalsLTGT :: Equals LT GT False
instance equalsGTLT :: Equals GT LT False
instance equalsGTEQ :: Equals GT EQ False

equals :: forall l r o. Equals l r o => OProxy l -> OProxy r -> BProxy o
equals _ _ = BProxy

