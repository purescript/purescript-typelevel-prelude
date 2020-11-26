module Type.Data.Boolean
  ( BProxy(..)
  , module Prim.Boolean
  , class IsBoolean
  , reflectBoolean
  , reifyBoolean
  , class Equals
  , equals
  , class And
  , and
  , class Or
  , or
  , class Not
  , not
  , class If
  , if_
  ) where

import Prim.Boolean (True, False)
import Type.Proxy (Proxy(..))

-- | Value proxy for `Boolean` types
-- | **Deprecated:** Use `Type.Proxy` instead
data BProxy :: Boolean -> Type
data BProxy bool = BProxy

-- | Class for reflecting a type level `Boolean` at the value level
class IsBoolean :: Boolean -> Constraint
class IsBoolean bool where
  reflectBoolean :: forall proxy. proxy bool -> Boolean

instance isBooleanTrue :: IsBoolean True where reflectBoolean _ = true
instance isBooleanFalse :: IsBoolean False where reflectBoolean _ = false

-- | Use a value level `Boolean` as a type-level `Boolean`
reifyBoolean :: forall r. Boolean -> (forall proxy o. IsBoolean o => proxy o -> r) -> r
reifyBoolean true f = f (Proxy :: Proxy True)
reifyBoolean false f = f (Proxy :: Proxy False)

class Equals :: Boolean -> Boolean -> Boolean -> Constraint
class Equals lhs rhs out | lhs rhs -> out, lhs out -> rhs, rhs out -> lhs

instance equalsTrueTrueTrue :: Equals True True True
else instance equalsTrueFalseFalse :: Equals True False False
else instance equalsFalseTrueFalse :: Equals False True False
else instance equalsFalseFalseTrue :: Equals False False True

equals :: forall proxy l r o. Equals l r o => proxy l -> proxy r -> Proxy o
equals _ _ = Proxy

-- | And two `Boolean` types together
class And :: Boolean -> Boolean -> Boolean -> Constraint
class And lhs rhs out | lhs rhs -> out
instance andTrue :: And True rhs rhs
instance andFalse :: And False rhs False

and :: forall l r o. And l r o => BProxy l -> BProxy r -> BProxy o
and _ _ = BProxy

-- | Or two `Boolean` types together
class Or :: Boolean -> Boolean -> Boolean -> Constraint
class Or lhs rhs output | lhs rhs -> output
instance orTrue :: Or True rhs True
instance orFalse :: Or False rhs rhs

or :: forall l r o. Or l r o => BProxy l -> BProxy r -> BProxy o
or _ _ = BProxy

-- | Not a `Boolean`
class Not :: Boolean -> Boolean -> Constraint
class Not bool output | bool -> output
instance notTrue :: Not True False
instance notFalse :: Not False True

not :: forall i o. Not i o => BProxy i -> BProxy o
not _ = BProxy

-- | If - dispatch based on a boolean
class If :: forall k. Boolean -> k -> k -> k -> Constraint
class If bool onTrue onFalse output | bool onTrue onFalse -> output
instance ifTrue :: If True onTrue onFalse onTrue
instance ifFalse :: If False onTrue onFalse onFalse

if_ :: forall b t e o. If b t e o => BProxy b -> Proxy t -> Proxy e -> Proxy o
if_ _ _ _ = Proxy
