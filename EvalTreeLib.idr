module EvalTreeLib

import IntervalLib
import Data.Vect
import Data.So
import WeightLib

public export
data LeafValue :Type where
      MkLeaf : (value:Double)->(Interval lower upper) ->
      So( (lower<=value) && (value <= upper)) -> LeafValue


public export
data  EvalTree = EvalLeaf (Maybe LeafValue)
               | EvalNode (WeightVect ws{n} )
                          (Vect n  EvalTree)
                          (Maybe Double)


public export
valLeaf  : Maybe LeafValue -> Maybe Double
valLeaf  Nothing = Nothing
valLeaf (Just (MkLeaf value (MkInterval lower upper x) y)) = Just value


public export
getLeafNumber :  EvalTree-> Nat
getLeafNumber (EvalLeaf x) = 1
getLeafNumber (EvalNode ws ys result) = let mapped = map getLeafNumber ys
                                         in sum mapped



public export
defaultInput : LeafValue
defaultInput = MkLeaf 0.0 (MkInterval 0 1 Oh) Oh



public export
setLeafsHelper : List (Maybe LeafValue) -> (Vect n EvalTree) -> (Vect n EvalTree)
setLeafsHelper [] [] = Nil
setLeafsHelper [] (x :: xs) = (x :: xs)
setLeafsHelper (x :: xs) [] = Nil
setLeafsHelper (x :: xs) ((EvalLeaf (Just y)) :: ys) = (EvalLeaf  x ) :: setLeafsHelper xs ys
setLeafsHelper (x :: xs) ((EvalNode vect{ws} ts result) :: ys) = let len = getLeafNumber (EvalNode vect{ws} ts result)
                                                                     partition = take len (x::xs)
                                                                     right_partition = drop len (x::xs)
                                                                  in (EvalNode vect{ws} (setLeafsHelper partition ts) result)::(setLeafsHelper right_partition ys)


public export
setLeafs : List (Maybe LeafValue) ->  EvalTree  -> EvalTree
setLeafs [] tree = tree
setLeafs (Nothing :: xs) (EvalLeaf (Just y))     = EvalLeaf Nothing
setLeafs ((Just x) :: xs) (EvalLeaf (Just y))    = EvalLeaf (Just x)
setLeafs (x :: xs) (EvalNode vect{ws} ts result) = EvalNode vect{ws} (setLeafsHelper (x :: xs) ts) result
