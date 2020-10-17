module Type.Function where

-- | Polymorphic Type application
-- |
-- | For example...
-- | ```
-- | APPLY Maybe Int == Maybe $ Int == Maybe Int
-- | ```
type APPLY :: forall a b. (a -> b) -> a -> b
type APPLY f a = f a

infixr 0 type APPLY as $

-- | Reversed polymorphic Type application
-- |
-- | For example...
-- | ```
-- | FLIP Int Maybe == Maybe Int
-- | ```
-- | Note: an infix for FLIP (e.g. `Int # Maybe`) is not allowed.
-- | Before the `0.14.0` release, we used `# Type` to refer to a row of types.
-- | In  the `0.14.0` release, the `# Type` syntax was deprecated,
-- | and `Row Type` is the correct way to do this now. To help mitigate
-- | breakage, `# Type` was made an alias to `Row Type`. When the `# Type`
-- | syntax is fully dropped in a later language release, we can then
-- | support the infix version: `Int # Maybe`.
type FLIP :: forall a b. a -> (a -> b) -> b
type FLIP a f = f a
