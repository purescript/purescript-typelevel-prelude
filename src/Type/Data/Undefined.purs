module Type.Data.Undefined (undefined) where

-- | An undefined value of any type.
-- |
-- | Useful for when a type level operation has no defined value.
-- |
-- | For example, if you're defining the type level successor operation
-- | for natural numbers, you're interested exclusively in the return
-- | type, so you would use `undefined` as the return value.
-- |
-- |     succ :: Nat n => n -> Succ n
-- |     succ _ = undefined
foreign import undefined :: forall a. a
