module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Symbol (class Cons)
import Type.Data.Symbol (class Reverse, class Snoc, reifySymbol, reverse, snoc)
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
  log "asd"