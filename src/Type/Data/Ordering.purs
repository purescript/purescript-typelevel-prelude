module Type.Data.Ordering
  ( kind Ordering
  , LT
  , EQ
  , GT
  , OProxy(..)
  , class IsOrdering
  , reflectOrdering
  , reifyOrdering
  , class AppendOrdering
  , appendOrdering
  , class InvertOrdering
  , invertOrdering
  , class Equals
  , equals
  ) where

import Data.Ordering (Ordering(..))
import Type.Data.Boolean (kind Boolean, True, False, BProxy(..))

foreign import kind Ordering
foreign import data LT :: Ordering
foreign import data EQ :: Ordering
foreign import data GT :: Ordering

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
class AppendOrdering (lhs :: Ordering)
                     (rhs :: Ordering)
                     (output :: Ordering) |
                     lhs -> rhs output
instance appendOrderingLT :: AppendOrdering LT rhs LT
instance appendOrderingEQ :: AppendOrdering EQ rhs rhs
instance appendOrderingGT :: AppendOrdering GT rhs GT

appendOrdering :: forall l r o. AppendOrdering l r o => OProxy l -> OProxy r -> OProxy o
appendOrdering _ _ = OProxy

-- | Invert an `Ordering`
class InvertOrdering (ordering :: Ordering)
                     (result :: Ordering) |
                     ordering -> result
instance invertOrderingLT :: InvertOrdering LT GT
instance invertOrderingEQ :: InvertOrdering EQ EQ
instance invertOrderingGT :: InvertOrdering GT LT

invertOrdering :: forall i o. InvertOrdering i o => OProxy i -> OProxy o
invertOrdering _ = OProxy

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

