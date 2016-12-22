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
  ) where

import Data.Ordering (Ordering(..))
import Unsafe.Coerce (unsafeCoerce)

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
reifyOrdering o f = coerce f { reflectOrdering: \_ -> o } OProxy where
  coerce
    :: (forall o'. IsOrdering o'                      => OProxy o' -> r)
    -> { reflectOrdering :: OProxy EQ -> Ordering } -> OProxy EQ -> r
  coerce = unsafeCoerce

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
