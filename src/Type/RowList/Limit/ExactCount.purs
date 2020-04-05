module Type.RowList.Limit.ExactCount
  ( module Type.Data.Peano
  , module Prim.RowList
  , class ExactCount
  , class ExactCountEmpty
  , class ExactCountZero
  , class ExactCountOne
  , class ExactCountTwo
  , class ExactCountThree
  , class ExactCountFour
  , class ExactCountFive
  , class ExactCountSix
  , class ExactCountSeven
  , class ExactCountEight
  , class ExactCountNine
  , class ExactCountTen
  ) where

import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Data.Peano (kind Int, Pos, Z, Succ, P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)

{- Example

-- Using: class ExactCount

oneRow1a :: forall r rl. RowToList r rl => ExactCount One rl => { | r } -> String
oneRow1a = ...

... oneRow1a { } -- Error: "Could not match type"
... oneRow1a { a: 1 } -- Works
... oneRow1a { a: 1, b: 2 } -- Error: "Could not match type"


-- Using Alias: class ExactCountOne

oneRow1b :: forall r rl. RowToList r rl => ExactCountOne rl => { | r } -> String
oneRow1b = ...

... oneRow1b { } -- Error: "Could not match type"
... oneRow1b { a: 1 } -- Works
... oneRow1b { a: 1, b: 2 } -- Error: "Could not match type"

-}

-- There is only one RowList match for each count
class ExactCount (count :: Int) (rl :: RowList) | count -> rl

-- Zero "Empty"
instance zeroLimitRow :: ExactCount (Pos Z) Nil

class (ExactCount (Pos Z) rl) <= ExactCountEmpty (rl :: RowList)
class (ExactCount (Pos Z) rl) <= ExactCountZero (rl :: RowList)
instance zeroExactCountEmpty :: ExactCountEmpty Nil
instance zeroExactCountZero :: ExactCountZero Nil

-- One
instance oneConsExactCount :: ExactCount (Pos (Succ Z))
  (Cons l1 t1 Nil)

class (ExactCount (Pos (Succ Z)) rl) <= ExactCountOne (rl :: RowList)
instance oneConsExactCountOne :: ExactCountOne (Cons l1 t1 Nil)

-- Two
instance twoConsExactCount :: ExactCount (Pos (Succ (Succ Z)))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )

class (ExactCount (Pos (Succ (Succ Z))) rl) <= ExactCountTwo (rl :: RowList)
instance twoConsExactCountTwo :: ExactCountTwo
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )

-- Three
instance threeConsExactCount :: ExactCount (Pos (Succ (Succ (Succ Z))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))

class (ExactCount (Pos (Succ (Succ (Succ Z)))) rl) <= ExactCountThree (rl :: RowList)
instance threeConsExactCountThree :: ExactCountThree
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))

-- Four
instance fourConsExactCount :: ExactCount (Pos (Succ (Succ (Succ (Succ Z)))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))

class (ExactCount (Pos (Succ (Succ (Succ (Succ Z))))) rl) <= ExactCountFour (rl :: RowList)
instance fourConsExactCountFour :: ExactCountFour
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))

-- Five
instance fiveConsExactCount :: ExactCount (Pos (Succ (Succ (Succ (Succ (Succ Z))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))

class (ExactCount (Pos (Succ (Succ (Succ (Succ (Succ Z)))))) rl) <= ExactCountFive (rl :: RowList)
instance fiveConsExactCountFive :: ExactCountFive
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))

-- Six
instance sixConsExactCount :: ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))

class (ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z))))))) rl) <= ExactCountSix (rl :: RowList)
instance sixConsExactCountSix :: ExactCountSix
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))

-- Seven
instance sevenConsExactCount :: ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))

class (ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))) rl) <= ExactCountSeven (rl :: RowList)
instance sevenConsExactCountSeven :: ExactCountSeven
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))

-- Eight
instance eightConsExactCount :: ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8 Nil)
  )))))))

class (ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))) rl) <= ExactCountEight (rl :: RowList)
instance eightConsExactCountEight :: ExactCountEight
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8 Nil)
  )))))))

-- Nine
instance nineConsExactCount :: ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8
                  (Cons l9 t9 Nil)
  ))))))))

class (ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))) rl) <= ExactCountNine (rl :: RowList)
instance nineConsExactCountNine :: ExactCountNine
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8
                  (Cons l9 t9 Nil)
  ))))))))

-- Ten
instance tenConsExactCount :: ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8
                  (Cons l9 t9
                    (Cons l10 t10 Nil)
  )))))))))

class (ExactCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))) rl) <= ExactCountTen (rl :: RowList)
instance tenConsExactCountTen :: ExactCountTen
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8
                  (Cons l9 t9
                    (Cons l10 t10 Nil)
  )))))))))