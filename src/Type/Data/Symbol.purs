module Type.Data.Symbol
  ( module Data.Symbol
  , class CompareSymbol
  , compareSymbol
  , class AppendSymbol
  , appendSymbol
  ) where

import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol, reifySymbol)
import Type.Data.Ordering (OProxy(..), kind Ordering)

-- | Compare two `Symbol` types
class CompareSymbol (lhs :: Symbol)
                    (rhs :: Symbol)
                    (out :: Ordering) |
                    lhs rhs -> out

compareSymbol :: forall l r o. CompareSymbol l r o => SProxy l -> SProxy r -> OProxy o
compareSymbol _ _ = OProxy


-- | Append two `Symbol` types together
class AppendSymbol (lhs :: Symbol)
                   (rhs :: Symbol)
                   (out :: Symbol) |
                   lhs rhs -> out

appendSymbol :: forall l r o. AppendSymbol l r o => SProxy l -> SProxy r -> SProxy o
appendSymbol _ _ = SProxy

