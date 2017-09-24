module Type.Row
  ( RProxy(..)
  , class RowLacks
  , class RowLacking
  , kind RowList
  , Nil
  , Cons
  , RLProxy(..)
  , class RowToList
  , class ListToRow
  , class RowLabels
  , labels
  , labelsR
  ) where


import Prelude ((<>))
import Type.Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

data RProxy (row :: # Type) = RProxy

-- Must not be exported
foreign import data Entry :: Type

-- | If you get "No type class instance was found" for this class, then your
-- | `row` shouldn't contain the label `key`.
class RowLacking (entry :: Type)
                 (key :: Symbol)
                 (typ :: Type)
                 (row :: # Type) |
                 entry typ -> key row

instance rowLacking :: RowLacking entry key entry row

-- | Encodes the constraint that a given row does not contain a specific key.
class RowLacks (key :: Symbol)
               (row :: # Type)

-- Append `Entry` at label `key` to the right of `row` then lookup `key` on the
-- left - if we check via instance solving that the `typ` we get back is
-- `Entry`, then `row` lacks `key`.  In the case that `row` doesn't lack
-- `key`, we get a "No type class instance found" error for:
-- `RowLacking Entry key typ row`.
instance rowLacks
  :: ( RowCons key Entry () keyEntry
     , Union row keyEntry rowKeyEntry
     , RowCons key typ ignored rowKeyEntry
     , RowLacking Entry key typ row )
  => RowLacks key row


-- | A type-level list representation of a row
foreign import kind RowList
foreign import data Nil :: RowList
foreign import data Cons :: Symbol -> Type -> RowList -> RowList

data RLProxy (rowList :: RowList) = RLProxy

-- | Extract the collection of entries in a closed row of types.
-- | The list of entries is sorted by label and preserves duplicates.
-- | The inverse of this operation is `ListToRow`.
-- | Solved by the compiler.
class RowToList (row :: # Type)
                (list :: RowList) |
                row -> list

-- | Convert a RowList to a row of types.
-- | The inverse of this operation is `RowToList`.
class ListToRow (list :: RowList)
                (row :: # Type) |
                list -> row

instance listToRowNil
  :: ListToRow Nil ()

instance listToRowCons
  :: ( ListToRow tail tailRow
     , RowCons label ty tailRow row )
  => ListToRow (Cons label ty tail) row

class RowLabels (l :: RowList) where
  labels
    :: RLProxy l
    -> Array String

instance getLabelsNil :: RowLabels Nil where
  labels _ = []

instance getLabelsCons1
  :: ( IsSymbol name1 )
  => RowLabels (Cons name1 ty1 Nil) where
  labels _ = [reflectSymbol (SProxy :: SProxy name1)]
instance getLabelsCons2
  :: ( IsSymbol name1
     , IsSymbol name2 )
  => RowLabels (Cons name1 ty1 (Cons name2 ty2 Nil)) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    ]
instance getLabelsCons3
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3 )
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3 Nil))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    ]
instance getLabelsCons4
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3
     , IsSymbol name4 )
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3
            (Cons name4 ty4 Nil)))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    , reflectSymbol (SProxy :: SProxy name4)
    ]
instance getLabelsCons5
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3
     , IsSymbol name4
     , IsSymbol name5 )
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3
            (Cons name4 ty4
              (Cons name5 ty5 Nil))))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    , reflectSymbol (SProxy :: SProxy name4)
    , reflectSymbol (SProxy :: SProxy name5)
    ]
instance getLabelsCons6
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3
     , IsSymbol name4
     , IsSymbol name5
     , IsSymbol name6 )
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3
            (Cons name4 ty4
              (Cons name5 ty5
                (Cons name6 ty6 Nil)))))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    , reflectSymbol (SProxy :: SProxy name4)
    , reflectSymbol (SProxy :: SProxy name5)
    , reflectSymbol (SProxy :: SProxy name6)
    ]
instance getLabelsCons7
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3
     , IsSymbol name4
     , IsSymbol name5
     , IsSymbol name6
     , IsSymbol name7 )
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3
            (Cons name4 ty4
              (Cons name5 ty5
                (Cons name6 ty6
                  (Cons name7 ty7 Nil))))))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    , reflectSymbol (SProxy :: SProxy name4)
    , reflectSymbol (SProxy :: SProxy name5)
    , reflectSymbol (SProxy :: SProxy name6)
    , reflectSymbol (SProxy :: SProxy name7)
    ]
instance getLabelsCons8
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3
     , IsSymbol name4
     , IsSymbol name5
     , IsSymbol name6
     , IsSymbol name7
     , IsSymbol name8 )
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3
            (Cons name4 ty4
              (Cons name5 ty5
                (Cons name6 ty6
                  (Cons name7 ty7
                    (Cons name8 ty8 Nil)))))))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    , reflectSymbol (SProxy :: SProxy name4)
    , reflectSymbol (SProxy :: SProxy name5)
    , reflectSymbol (SProxy :: SProxy name6)
    , reflectSymbol (SProxy :: SProxy name7)
    , reflectSymbol (SProxy :: SProxy name8)
    ]
instance getLabelsCons9
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3
     , IsSymbol name4
     , IsSymbol name5
     , IsSymbol name6
     , IsSymbol name7
     , IsSymbol name8
     , IsSymbol name9 )
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3
            (Cons name4 ty4
              (Cons name5 ty5
                (Cons name6 ty6
                  (Cons name7 ty7
                    (Cons name8 ty8
                      (Cons name9 ty9 Nil))))))))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    , reflectSymbol (SProxy :: SProxy name4)
    , reflectSymbol (SProxy :: SProxy name5)
    , reflectSymbol (SProxy :: SProxy name6)
    , reflectSymbol (SProxy :: SProxy name7)
    , reflectSymbol (SProxy :: SProxy name8)
    , reflectSymbol (SProxy :: SProxy name9)
    ]
instance getLabelsCons10
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3
     , IsSymbol name4
     , IsSymbol name5
     , IsSymbol name6
     , IsSymbol name7
     , IsSymbol name8
     , IsSymbol name9
     , IsSymbol name10 )
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3
            (Cons name4 ty4
              (Cons name5 ty5
                (Cons name6 ty6
                  (Cons name7 ty7
                    (Cons name8 ty8
                      (Cons name9 ty9
                        (Cons name10 ty10 Nil)))))))))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    , reflectSymbol (SProxy :: SProxy name4)
    , reflectSymbol (SProxy :: SProxy name5)
    , reflectSymbol (SProxy :: SProxy name6)
    , reflectSymbol (SProxy :: SProxy name7)
    , reflectSymbol (SProxy :: SProxy name8)
    , reflectSymbol (SProxy :: SProxy name9)
    , reflectSymbol (SProxy :: SProxy name10)
    ]
instance getLabelsCons
  :: ( IsSymbol name1
     , IsSymbol name2
     , IsSymbol name3
     , IsSymbol name4
     , IsSymbol name5
     , IsSymbol name6
     , IsSymbol name7
     , IsSymbol name8
     , IsSymbol name9
     , IsSymbol name10
     , IsSymbol name11
     , RowLabels rest)
  => RowLabels
      (Cons name1 ty1
        (Cons name2 ty2
          (Cons name3 ty3
            (Cons name4 ty4
              (Cons name5 ty5
                (Cons name6 ty6
                  (Cons name7 ty7
                    (Cons name8 ty8
                      (Cons name9 ty9
                        (Cons name10 ty10
                          (Cons name11 ty11 rest))))))))))) where
  labels _ =
    [ reflectSymbol (SProxy :: SProxy name1)
    , reflectSymbol (SProxy :: SProxy name2)
    , reflectSymbol (SProxy :: SProxy name3)
    , reflectSymbol (SProxy :: SProxy name4)
    , reflectSymbol (SProxy :: SProxy name5)
    , reflectSymbol (SProxy :: SProxy name6)
    , reflectSymbol (SProxy :: SProxy name7)
    , reflectSymbol (SProxy :: SProxy name8)
    , reflectSymbol (SProxy :: SProxy name9)
    , reflectSymbol (SProxy :: SProxy name10)
    , reflectSymbol (SProxy :: SProxy name11)
    ] <> labels (RLProxy :: RLProxy rest)

labelsR
  :: forall r l
   . RowToList r l
  => RowLabels l
  => RProxy r
  -> Array String
labelsR _ = labels (RLProxy :: RLProxy l)
