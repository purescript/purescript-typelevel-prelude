module Type.Data.Boolean
  ( kind Boolean
  , True
  , False
  , BProxy(..)
  , class IsBoolean
  , reflectBoolean
  , reifyBoolean
  , class And
  , and
  , class Or
  , or
  , class Not
  , not
  , class If
  , if_
  ) where

import Type.Proxy (Proxy(..))

foreign import kind Boolean
foreign import data True :: Boolean
foreign import data False :: Boolean

-- | Value proxy for `Boolean` types
data BProxy (bool :: Boolean) = BProxy

-- | Class for reflecting a type level `Boolean` at the value level
class IsBoolean (bool :: Boolean) where
  reflectBoolean :: BProxy bool -> Boolean

instance isBooleanTrue :: IsBoolean True where reflectBoolean _ = true
instance isBooleanFalse :: IsBoolean False where reflectBoolean _ = false

-- | Use a value level `Boolean` as a type-level `Boolean`
reifyBoolean :: forall r. Boolean -> (forall o. IsBoolean o => BProxy o -> r) -> r
reifyBoolean true f = f (BProxy :: BProxy True)
reifyBoolean false f = f (BProxy :: BProxy False)

-- | And two `Boolean` types together
class And (lhs :: Boolean)
          (rhs :: Boolean)
          (output :: Boolean) |
          lhs -> rhs output
instance andTrue :: And True rhs rhs
instance andFalse :: And False rhs False

and :: forall l r o. And l r o => BProxy l -> BProxy r -> BProxy o
and _ _ = BProxy

-- | Or two `Boolean` types together
class Or (lhs :: Boolean)
         (rhs :: Boolean)
         (output :: Boolean) |
         lhs -> rhs output
instance orTrue :: Or True rhs True
instance orFalse :: Or False rhs rhs

or :: forall l r o. Or l r o => BProxy l -> BProxy r -> BProxy o
or _ _ = BProxy

-- | Not a `Boolean`
class Not (bool :: Boolean)
          (output :: Boolean) |
          bool -> output
instance notTrue :: Not True False
instance notFalse :: Not False True

not :: forall i o. Not i o => BProxy i -> BProxy o
not _ = BProxy

-- | If - dispatch based on a boolean
class If (bool :: Boolean)
         (onTrue :: Type)
         (onFalse :: Type)
         (output :: Type) |
         bool -> onTrue onFalse output
instance ifTrue :: If True onTrue onFalse onTrue
instance ifFalse :: If False onTrue onFalse onFalse

if_ :: forall b t e o. If b t e o => BProxy b -> Proxy t -> Proxy e -> Proxy o
if_ _ _ _ = Proxy
