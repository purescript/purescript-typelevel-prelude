module Type.RowList.Limit.LimitCount
  ( module Type.Data.Peano
  , module Prim.RowList
  , class LimitCount
  , class LimitCountEmpty
  , class LimitCountZero
  , class LimitCountOne
  , class LimitCountTwo
  , class LimitCountThree
  , class LimitCountFour
  , class LimitCountFive
  , class LimitCountSix
  , class LimitCountSeven
  , class LimitCountEight
  , class LimitCountNine
  , class LimitCountTen
  ) where

import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Data.Peano (kind Int, Pos, Z, Succ, P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)

{- Example

-- Using: class LimitCount

oneRow1a :: forall r rl. RowToList r rl => LimitCount One rl => { | r } -> String
oneRow1a = ...

... oneRow1a { } -- Works
... oneRow1a { a: 1 } -- Works
... oneRow1a { a: 1, b: 2 } -- Error: "Could not match type"


-- Using Alias: class LimitCountThree

oneRow1b :: forall r rl. RowToList r rl => LimitCountThree rl => { | r } -> String
oneRow1b = ...

... oneRow1b { } works
... oneRow1b { a: 1 } -- Works
... oneRow1b { a: 1, b: 2 } -- Works
... oneRow1b { a: 1, b: 2, c: 3 } -- Works
... oneRow1b { a: 1, b: 2, c: 3, d: 4 } -- Error: "Could not match type"

-}

-- There is only one RowList match for each count
class LimitCount (count :: Int) (rl :: RowList) | count -> rl

-- Zero "Empty"
instance zeroLimitRow :: LimitCount (Pos Z) Nil

class (LimitCount (Pos Z) rl) <= LimitCountZero (rl :: RowList)
instance zeroLimitCountZero :: LimitCountZero Nil

class (LimitCount (Pos Z) rl) <= LimitCountEmpty (rl :: RowList)
instance zeroLimitCountEmpty :: LimitCountEmpty Nil

-- One
instance zeroConsLimitCountOne  :: LimitCount (Pos (Succ Z))
  Nil
else instance oneConsLimitCountOne  :: LimitCount (Pos (Succ Z))
  (Cons l1 t1 Nil)

class LimitCountOne (rl :: RowList)
instance zeroConsLimitCountOneAlias  :: LimitCountOne
  Nil
instance oneConsLimitCountOneAlias  :: LimitCountOne
  (Cons l1 t1 Nil)

-- Two
instance zeroConsLimitCountTwo  :: LimitCount (Pos (Succ (Succ Z)))
  Nil
else instance oneConsLimitCountTwo  :: LimitCount (Pos (Succ (Succ Z)))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountTwo  :: LimitCount (Pos (Succ (Succ Z)))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )

class LimitCountTwo (rl :: RowList)
instance zeroConsLimitCountTwoAlias  :: LimitCountTwo
  Nil
instance oneConsLimitCountTwoAlias  :: LimitCountTwo
  (Cons l1 t1 Nil)
instance twoConsLimitCountTwoAlias  :: LimitCountTwo
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )

-- Three
instance zeroConsLimitCountThree  :: LimitCount (Pos (Succ (Succ (Succ Z))))
  Nil
else instance oneConsLimitCountThree  :: LimitCount (Pos (Succ (Succ (Succ Z))))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountThree  :: LimitCount (Pos (Succ (Succ (Succ Z))))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
else instance threeConsLimitCountThree  :: LimitCount (Pos (Succ (Succ (Succ Z))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))

class LimitCountThree (rl :: RowList)
instance zeroConsLimitCountThreeAlias  :: LimitCountThree
  Nil
instance oneConsLimitCountThreeAlias  :: LimitCountThree
  (Cons l1 t1 Nil)
instance twoConsLimitCountThreeAlias  :: LimitCountThree
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
instance threeConsLimitCountThreeAlias  :: LimitCountThree
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))

-- Four
instance zeroConsLimitCountFour  :: LimitCount (Pos (Succ (Succ (Succ (Succ Z)))))
  Nil
else instance oneConsLimitCountFour  :: LimitCount (Pos (Succ (Succ (Succ (Succ Z)))))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountFour  :: LimitCount (Pos (Succ (Succ (Succ (Succ Z)))))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
else instance threeConsLimitCountFour  :: LimitCount (Pos (Succ (Succ (Succ (Succ Z)))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
 else instance fourConsLimitCountFour  :: LimitCount (Pos (Succ (Succ (Succ (Succ Z)))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))

class LimitCountFour (rl :: RowList)
instance zeroConsLimitCountFourAlias  :: LimitCountFour
  Nil
instance oneConsLimitCountFourAlias  :: LimitCountFour
  (Cons l1 t1 Nil)
instance twoConsLimitCountFourAlias  :: LimitCountFour
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
instance threeConsLimitCountFourAlias  :: LimitCountFour
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
instance fourConsLimitCountFourAlias  :: LimitCountFour
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))

-- Five
instance zeroConsLimitCountFive  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ Z))))))
  Nil
else instance oneConsLimitCountFive  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ Z))))))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountFive  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ Z))))))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
else instance threeConsLimitCountFive  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ Z))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
 else instance fourConsLimitCountFive  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ Z))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
 else instance fiveConsLimitCountFive  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ Z))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))

class LimitCountFive (rl :: RowList)
instance zeroConsLimitCountFiveAlias  :: LimitCountFive
  Nil
instance oneConsLimitCountFiveAlias  :: LimitCountFive
  (Cons l1 t1 Nil)
instance twoConsLimitCountFiveAlias  :: LimitCountFive
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
instance threeConsLimitCountFiveAlias  :: LimitCountFive
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
instance fourConsLimitCountFiveAlias  :: LimitCountFive
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
instance fiveConsLimitCountFiveAlias  :: LimitCountFive
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))

-- Six
instance zeroConsLimitCountSix  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
  Nil
else instance oneConsLimitCountSix  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountSix  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
else instance threeConsLimitCountSix  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
 else instance fourConsLimitCountSix  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
 else instance fiveConsLimitCountSix  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
 else instance sixConsLimitCountSix  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))

class LimitCountSix (rl :: RowList)
instance zeroConsLimitCountSixAlias  :: LimitCountSix
  Nil
instance oneConsLimitCountSixAlias  :: LimitCountSix
  (Cons l1 t1 Nil)
instance twoConsLimitCountSixAlias  :: LimitCountSix
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
instance threeConsLimitCountSixAlias  :: LimitCountSix
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
instance fourConsLimitCountSixAlias  :: LimitCountSix
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
instance fiveConsLimitCountSixAlias  :: LimitCountSix
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
instance sixConsLimitCountSixAlias  :: LimitCountSix
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))

-- Seven
instance zeroConsLimitCountSeven  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  Nil
else instance oneConsLimitCountSeven  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountSeven  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
else instance threeConsLimitCountSeven  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
 else instance fourConsLimitCountSeven  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
 else instance fiveConsLimitCountSeven  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
 else instance sixConsLimitCountSeven  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))
 else instance sevenConsLimitCountSeven  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))

class LimitCountSeven (rl :: RowList)
instance zeroConsLimitCountSevenAlias  :: LimitCountSeven
  Nil
instance oneConsLimitCountSevenAlias  :: LimitCountSeven
  (Cons l1 t1 Nil)
instance twoConsLimitCountSevenAlias  :: LimitCountSeven
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
instance threeConsLimitCountSevenAlias  :: LimitCountSeven
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
instance fourConsLimitCountSevenAlias  :: LimitCountSeven
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
instance fiveConsLimitCountSevenAlias  :: LimitCountSeven
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
instance sixConsLimitCountSevenAlias  :: LimitCountSeven
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))
instance sevenConsLimitCountSevenAlias  :: LimitCountSeven
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))

-- Eight
instance zeroConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  Nil
else instance oneConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
else instance threeConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
 else instance fourConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
 else instance fiveConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
 else instance sixConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))
 else instance sevenConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))
 else instance eightConsLimitCountEight  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8 Nil)
  )))))))

class LimitCountEight (rl :: RowList)
instance zeroConsLimitCountEightAlias  :: LimitCountEight
  Nil
instance oneConsLimitCountEightAlias  :: LimitCountEight
  (Cons l1 t1 Nil)
instance twoConsLimitCountEightAlias  :: LimitCountEight
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
instance threeConsLimitCountEightAlias  :: LimitCountEight
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
instance fourConsLimitCountEightAlias  :: LimitCountEight
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
instance fiveConsLimitCountEightAlias  :: LimitCountEight
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
instance sixConsLimitCountEightAlias  :: LimitCountEight
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))
instance sevenConsLimitCountEightAlias  :: LimitCountEight
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))
instance eightConsLimitCountEightAlias  :: LimitCountEight
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
instance zeroConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  Nil
else instance oneConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
else instance threeConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
 else instance fourConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
 else instance fiveConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
 else instance sixConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))
 else instance sevenConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))
 else instance eightConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8 Nil)
  )))))))
 else instance nineConsLimitCountNine  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))
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

class LimitCountNine (rl :: RowList)
instance zeroConsLimitCountNineAlias  :: LimitCountNine
  Nil
instance oneConsLimitCountNineAlias  :: LimitCountNine
  (Cons l1 t1 Nil)
instance twoConsLimitCountNineAlias  :: LimitCountNine
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
instance threeConsLimitCountNineAlias  :: LimitCountNine
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
instance fourConsLimitCountNineAlias  :: LimitCountNine
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
instance fiveConsLimitCountNineAlias  :: LimitCountNine
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
instance sixConsLimitCountNineAlias  :: LimitCountNine
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))
instance sevenConsLimitCountNineAlias  :: LimitCountNine
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))
instance eightConsLimitCountNineAlias  :: LimitCountNine
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8 Nil)
  )))))))
instance nineConsLimitCountNineAlias  :: LimitCountNine
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
instance zeroConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  Nil
else instance oneConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1 Nil)
else instance twoConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
else instance threeConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
 else instance fourConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
 else instance fiveConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
 else instance sixConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))
 else instance sevenConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))
 else instance eightConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8 Nil)
  )))))))
 else instance nineConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
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
 else instance tenConsLimitCountTen  :: LimitCount (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))
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

class LimitCountTen (rl :: RowList)
instance zeroConsLimitCountTenAlias  :: LimitCountTen
  Nil
instance oneConsLimitCountTenAlias  :: LimitCountTen
  (Cons l1 t1 Nil)
instance twoConsLimitCountTenAlias  :: LimitCountTen
  (Cons l1 t1
    (Cons l2 t2 Nil)
  )
instance threeConsLimitCountTenAlias  :: LimitCountTen
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3 Nil)
  ))
instance fourConsLimitCountTenAlias  :: LimitCountTen
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4 Nil)
  )))
instance fiveConsLimitCountTenAlias  :: LimitCountTen
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5 Nil)
  ))))
instance sixConsLimitCountTenAlias  :: LimitCountTen
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6 Nil)
  )))))
instance sevenConsLimitCountTenAlias  :: LimitCountTen
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7 Nil)
  ))))))
instance eightConsLimitCountTenAlias  :: LimitCountTen
  (Cons l1 t1
    (Cons l2 t2
      (Cons l3 t3
        (Cons l4 t4
          (Cons l5 t5
            (Cons l6 t6
              (Cons l7 t7
                (Cons l8 t8 Nil)
  )))))))
instance nineConsLimitCountTenAlias  :: LimitCountTen
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
instance tenConsLimitCountTenAlias  :: LimitCountTen
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