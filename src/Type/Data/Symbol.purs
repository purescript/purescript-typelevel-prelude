module Type.Data.Symbol
  ( module Data.Symbol
  , class CompareSymbol
  , compareSymbol
  , class AppendSymbol
  , appendSymbol
  , class Equals
  , equals
  , kind SList
  , SListProxy(..)
  , SNil
  , SCons
  , class Sort
  , sort
  ) where

import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol, reifySymbol)
import Type.Data.Ordering (OProxy(..), kind Ordering, EQ, LT, GT)
import Type.Data.Ordering (class Equals) as Ordering
import Type.Data.Boolean (kind Boolean, BProxy(..))

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

class Equals (lhs :: Symbol)
             (rhs :: Symbol)
             (out :: Boolean) |
             lhs rhs -> out

instance equalsSymbol
  :: (CompareSymbol lhs rhs ord,
      Ordering.Equals EQ ord out)
  => Equals lhs rhs out

equals :: forall l r o. Equals l r o => SProxy l -> SProxy r -> BProxy o
equals _ _ = BProxy

-- | A type-level list of Symbols
foreign import kind SList
foreign import data SNil :: SList
foreign import data SCons :: Symbol -> SList -> SList

-- | Proxy specialised for SLists
data SListProxy (l :: SList) = SListProxy

-- | Sort a type-level list of Symbols in alphabetically ascending order
class Sort (i :: SList) (o :: SList) | i -> o

instance sortSNil  :: Sort SNil SNil
instance sortSCons :: (Insert x ys xs', Sort xs ys) => Sort (SCons x xs) xs'

-- Insert a Symbol into an ordered list
class Insert (v :: Symbol) (i :: SList) (o :: SList) | i v -> o

instance insertSNil :: Insert x SNil (SCons x SNil)
instance insertSCons
  :: ( CompareSymbol x y c
     , InsertI c x y ys o
     )
  => Insert x (SCons y ys) o

-- The actual insertion function, pattern matching on the result of the comparison
class InsertI (c :: Ordering)
              (x :: Symbol)
              (y :: Symbol)
              (ys :: SList)
              (o :: SList) |
              c x y ys -> o

instance insertLT :: InsertI LT x y ys (SCons x (SCons y ys))
instance insertEQ :: InsertI EQ x y ys (SCons x (SCons y ys))
instance insertGT :: Insert x ys xys => InsertI GT x y ys (SCons y xys)

sort :: forall xs ys. Sort xs ys => SListProxy xs -> SListProxy ys
sort _ = SListProxy
