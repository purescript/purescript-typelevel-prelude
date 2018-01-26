module Type.Equality
  ( class TypeEquals
  , to
  , from
  , class TypeEqualsBool
  ) where

import Type.Data.Boolean (kind Boolean, True, False)

-- | This type class asserts that types `a` and `b`
-- | are equal.
-- |
-- | The functional dependencies and the single
-- | instance below will force the two type arguments
-- | to unify when either one is known.
-- |
-- | Note: any instance will necessarily overlap with
-- | `refl` below, so instances of this class should
-- | not be defined in libraries.
class TypeEquals a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a

instance refl :: TypeEquals a a where
  to a = a
  from a = a

-- | This type class asserts that types `a` and `b`
-- | are or are not equal, depending on the type of `o`.
-- |
-- | `o` will be either `True`, if `a` is `b`,
-- | or `False` otherwise.
-- |
-- | When `o` is specified to be `True`/`False` then this type class
-- | acts as an assertion that `a` and `b` are equal/not equal, respectively.
-- |
-- | Instances should not be defined in libraries.
class TypeEqualsBool a b (o :: Boolean) | a b -> o

instance reflTypeEqualsBool :: TypeEqualsBool a a True
else instance notTypeEqualsBool :: TypeEqualsBool a b False
