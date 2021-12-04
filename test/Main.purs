module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Type.Data.Symbol (mirror, reifySymbol, snoc)
import Type.Proxy (Proxy(..))

-- mirror
testMirror :: Proxy "lobmys"
testMirror = mirror (Proxy::_ "symbol")

testMirror1 :: Proxy _
testMirror1 = mirror (Proxy::_ "symbol")

testMirror2 :: Proxy "lobmyys"
testMirror2 = mirror (Proxy :: Proxy _)

-- snoc
testSnoc :: Proxy "symbol"
testSnoc = snoc (Proxy :: Proxy "symbo") (Proxy :: Proxy "l")

testSnoc1 :: Proxy _
testSnoc1 = snoc (Proxy :: Proxy "symbo") (Proxy :: Proxy "l")

testSnoc2 :: Proxy "symbol"
testSnoc2 = snoc (Proxy :: Proxy _) (Proxy :: Proxy "l")

testSnoc3 :: Proxy "symbol"
testSnoc3 = snoc (Proxy :: Proxy "symbo") (Proxy :: Proxy _)

testSnoc4 :: Proxy "s"
testSnoc4 = snoc (Proxy :: Proxy "") (Proxy :: Proxy "s")


main :: Effect Unit
main = do
  log "asd"