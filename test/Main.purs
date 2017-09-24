module Test.Main where

import Prelude (Unit, const, discard, unit, ($), (==))
import Type.Row (Cons, Nil, RLProxy(..), labels)

type AlmostEff = Unit -> Unit

foreign import throwErr :: String -> AlmostEff

assert :: String -> Boolean -> AlmostEff
assert msg condition = if condition then const unit else throwErr msg

main :: AlmostEff
main = do
  assert "should be []"
    $ labels (RLProxy :: RLProxy Nil) == []
  assert "should be [\"x1\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int Nil)) == ["x1"]
  assert "should be [\"x1\", \"x2\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int Nil))) == ["x1", "x2"]
  assert "should be [\"x1\", \"x2\", \"x3\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int Nil)))) == ["x1", "x2", "x3"]
  assert "should be [\"x1\", \"x2\", \"x3\", \"x4\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int (Cons "x4" Int Nil))))) == ["x1", "x2", "x3", "x4"]
  assert "should be [\"x1\", \"x2\", \"x3\", \"x4\", \"x5\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int (Cons "x4" Int (Cons "x5" Int Nil)))))) == ["x1", "x2", "x3", "x4", "x5"]
  assert "should be [\"x1\", \"x2\", \"x3\", \"x4\", \"x5\", \"x6\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int (Cons "x4" Int (Cons "x5" Int (Cons "x6" Int Nil))))))) == ["x1", "x2", "x3", "x4", "x5", "x6"]
  assert "should be [\"x1\", \"x2\", \"x3\", \"x4\", \"x5\", \"x6\", \"x8\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int (Cons "x4" Int (Cons "x5" Int (Cons "x6" Int (Cons "x7" Int Nil)))))))) == ["x1", "x2", "x3", "x4", "x5", "x6", "x7"]
  assert "should be [\"x1\", \"x2\", \"x3\", \"x4\", \"x5\", \"x6\", \"x7\", \"x8\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int (Cons "x4" Int (Cons "x5" Int (Cons "x6" Int (Cons "x7" Int (Cons "x8" Int Nil))))))))) == ["x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"]
  assert "should be [\"x1\", \"x2\", \"x3\", \"x4\", \"x5\", \"x6\", \"x7\", \"x8\", \"x9\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int (Cons "x4" Int (Cons "x5" Int (Cons "x6" Int (Cons "x7" Int (Cons "x8" Int (Cons "x9" Int Nil)))))))))) == ["x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"]
  assert "should be [\"x1\", \"x2\", \"x3\", \"x4\", \"x5\", \"x6\", \"x7\", \"x8\", \"x9\", \"x10\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int (Cons "x4" Int (Cons "x5" Int (Cons "x6" Int (Cons "x7" Int (Cons "x8" Int (Cons "x9" Int (Cons "x10" Int Nil))))))))))) == ["x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10"]
  assert "should be [\"x1\", \"x2\", \"x3\", \"x4\", \"x5\", \"x6\", \"x7\", \"x8\", \"x9\", \"x10\", \"x11\"]"
    $ labels (RLProxy :: RLProxy (Cons "x1" Int (Cons "x2" Int (Cons "x3" Int (Cons "x4" Int (Cons "x5" Int (Cons "x6" Int (Cons "x7" Int (Cons "x8" Int (Cons "x9" Int (Cons "x10" Int (Cons "x11" Int Nil)))))))))))) == ["x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11"]
