module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Type.Data.Symbol (SProxy(..), mirror, snoc)

-- mirror
testMirror :: SProxy "lobmys"
testMirror = mirror (SProxy::_ "symbol")

testMirror1 :: SProxy _
testMirror1 = mirror (SProxy::_ "symbol")

testMirror2 :: SProxy "lobmyys"
testMirror2 = mirror (SProxy :: SProxy _)

-- snoc
testSnoc :: SProxy "symbol"
testSnoc = snoc (SProxy :: SProxy "symbo") (SProxy :: SProxy "l")

testSnoc1 :: SProxy _
testSnoc1 = snoc (SProxy :: SProxy "symbo") (SProxy :: SProxy "l")

testSnoc2 :: SProxy "symbol"
testSnoc2 = snoc (SProxy :: SProxy _) (SProxy :: SProxy "l")

testSnoc3 :: SProxy "symbol"
testSnoc3 = snoc (SProxy :: SProxy "symbo") (SProxy :: SProxy _)

testSnoc4 :: SProxy "s"
testSnoc4 = snoc (SProxy :: SProxy "") (SProxy :: SProxy "s")


main :: Effect Unit
main = do
  log "asd"