module Test.Main where

import Prelude

import Debug (spy)
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Symbol (class Cons)
import Test.Assert (assertTrue)
import Type.Data.Symbol (class IsSymbol, class Reverse, class ReverseP, class Snoc, reflectSymbol, reverse, snoc)
import Type.Proxy (Proxy(..))

-- reverse
testReverse :: Proxy "lobmys"
testReverse = reverse (Proxy :: Proxy "symbol")

testReverseForward :: Proxy _
testReverseForward = reverse (Proxy:: Proxy "symbol")

testReverseBackward :: Proxy "lobmyys"
testReverseBackward = reverse (Proxy :: Proxy _)

testReverseSingle :: Proxy "s"
testReverseSingle = reverse $ Proxy :: Proxy "s"

testReverseEmpty :: Proxy ""
testReverseEmpty = reverse $ Proxy :: Proxy ""

-- Reversing a reversed Symbol should be the same as the Symbol itself
propReverseIdem :: forall a b . IsSymbol a => IsSymbol b => Reverse a b => Reverse b a => Proxy a -> Boolean
propReverseIdem p = reflectSymbol p == reflectSymbol sameThing
  where
  sameThing = reverse (reverse p :: Proxy b)

testUnicode :: Proxy "🍎enip"
testUnicode = reverse (Proxy :: Proxy "pine🍎")

-- snoc
testSnoc :: Proxy "symbol"
testSnoc = snoc (Proxy :: Proxy "symbo") (Proxy :: Proxy "l")

testSnocForward :: Proxy _
testSnocForward = snoc (Proxy :: Proxy "symbo") (Proxy :: Proxy "l")

testSnocBackwardLeft :: Proxy "symbol"
testSnocBackwardLeft = snoc (Proxy :: Proxy _) (Proxy :: Proxy "l")

testSnocBackwardRight :: Proxy "symbol"
testSnocBackwardRight = snoc (Proxy :: Proxy "symbo") (Proxy :: Proxy _)

testSnocBackwardBoth :: Proxy "symbol"
testSnocBackwardBoth = snoc (Proxy :: Proxy _) (Proxy :: Proxy _)

testSnocEmpty :: Proxy "s"
testSnocEmpty = snoc (Proxy :: Proxy "") (Proxy :: Proxy "s")

main :: Effect Unit
main = do
  assertTrue $ propReverseIdem (Proxy :: Proxy "Test Symbol")
  assertTrue $ propReverseIdem (Proxy :: Proxy "Unico∀e")
  assertTrue $ propReverseIdem (Proxy :: Proxy "<-<>")
  assertTrue $ propReverseIdem (Proxy :: Proxy "pine🍎")