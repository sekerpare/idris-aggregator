module AggregationLib

import Data.Vect
import Data.So
import IntervalLib
import WeightLib
import EvalTreeLib



public export
data Norm = Single (Vect 2 Double)
          | Plural (List Double)

public export
data Op = Sum
        | Max
        | Average
        | Min



public export
vectToMaybeVect : Vect n Double ->  Vect n (Maybe Double)
vectToMaybeVect [] = []
vectToMaybeVect (x :: xs) = [Just x] ++ vectToMaybeVect xs



public export
sumOfMaybeVect :Vect m (Maybe Double) ->Double
sumOfMaybeVect [] = 0
sumOfMaybeVect (Nothing  :: xs) = sumOfMaybeVect xs
sumOfMaybeVect ((Just x) :: xs) = x + sumOfMaybeVect xs




public export
totalExeptGiven:Fin n-> Vect n (Maybe Double) ->Double
totalExeptGiven FZ [] impossible
totalExeptGiven (FS _) [] impossible
totalExeptGiven ind (weight::weights) = let dropped = deleteAt ind (weight::weights)
                                         in sumOfMaybeVect dropped


public export
addToEveryWeights: Double -> Vect n (Maybe Double) ->  Vect n (Maybe Double)
addToEveryWeights x []  = []
addToEveryWeights x (w :: ws) =
  case w of
    Nothing      => Nothing::(addToEveryWeights x ws)
    (Just value) => Just (value+(value*x))::(addToEveryWeights x ws)


public export
changeWeights : List (Fin n) -> Vect n (Maybe Double) ->  Vect n (Maybe Double)
changeWeights [] [] = []
changeWeights (FZ :: _) [] impossible
changeWeights ((FS _) :: _) [] impossible
changeWeights [] (weight :: weights) = (weight :: weights)
changeWeights (ind :: inds) (weight :: weights) =

    let element   = index ind (weight :: weights)
     in case element of
       Nothing    => changeWeights inds (weight :: weights)
       Just value => let replacedWs= replaceAt ind (Nothing) (weight :: weights)
                         totalEx   = totalExeptGiven ind (weight :: weights)
                         addedWs   = addToEveryWeights (value/totalEx) replacedWs
                      in changeWeights inds addedWs

public export
zipCaseOfNothing : Vect n (Maybe Double) -> Vect n (Maybe Double) ->  Vect n (Maybe Double)
zipCaseOfNothing [] []  = []
zipCaseOfNothing (Nothing :: inputs) (Nothing :: weights)     = Nothing :: (zipCaseOfNothing inputs weights )
zipCaseOfNothing (Nothing :: inputs) ((Just wVal) :: weights) = Nothing :: (zipCaseOfNothing inputs weights )
zipCaseOfNothing ((Just iVal) :: inputs) (Nothing :: weights) = Nothing :: (zipCaseOfNothing inputs weights )
zipCaseOfNothing ((Just iVal) :: inputs) ((Just wVal) :: weights) = (Just (iVal*wVal)) :: (zipCaseOfNothing inputs weights )


public export
isNothingVect : Vect n (Maybe Double) -> Bool
isNothingVect [] = True
isNothingVect (Nothing :: xs) = isNothingVect xs
isNothingVect ((Just x) :: xs) = False


public export
getResult: EvalTree -> Maybe Double
getResult (EvalLeaf x)           = valLeaf x
getResult (EvalNode weights children result) = result

public export
minOfMaybeVectHelper:Vect m (Maybe Double)->Maybe Double->Maybe Double
minOfMaybeVectHelper [] x = x
minOfMaybeVectHelper (Nothing :: xs)  Nothing  = minOfMaybeVectHelper xs Nothing
minOfMaybeVectHelper ((Just x) :: xs) Nothing  = minOfMaybeVectHelper xs (Just x)
minOfMaybeVectHelper (Nothing :: xs)  (Just x) = minOfMaybeVectHelper xs (Just x)
minOfMaybeVectHelper ((Just y) :: xs) (Just x) = case x<y of
                                                  True  => minOfMaybeVectHelper xs (Just x)
                                                  False => minOfMaybeVectHelper xs (Just y)


public export
minOfMaybeVect :Vect m (Maybe Double) -> Maybe Double
minOfMaybeVect [] = Nothing
minOfMaybeVect (x :: xs) = minOfMaybeVectHelper xs x



public export
maxOfMaybeVectHelper:Vect m (Maybe Double)->Maybe Double->Maybe Double
maxOfMaybeVectHelper [] x = x
maxOfMaybeVectHelper (Nothing :: xs)  Nothing  = maxOfMaybeVectHelper xs Nothing
maxOfMaybeVectHelper ((Just x) :: xs) Nothing  = maxOfMaybeVectHelper xs (Just x)
maxOfMaybeVectHelper (Nothing :: xs)  (Just x) = maxOfMaybeVectHelper xs (Just x)
maxOfMaybeVectHelper ((Just y) :: xs) (Just x) = case x>y of
                                                  True  => maxOfMaybeVectHelper xs (Just x)
                                                  False => maxOfMaybeVectHelper xs (Just y)


public export
maxOfMaybeVect :Vect m (Maybe Double) -> Maybe Double
maxOfMaybeVect [] = Nothing
maxOfMaybeVect (x :: xs) = maxOfMaybeVectHelper xs x


public export
avgOfMaybeVect :Vect m (Maybe Double) -> Maybe Double
avgOfMaybeVect xs{m} = case isNothingVect xs of
                        True => Nothing
                        False=> let s = sumOfMaybeVect xs
                                 in Just (s/(cast)m)


public export
arrange : Vect n Double -> Vect n( Maybe Double)  ->Vect n( Maybe Double)
arrange weights{n} aggVals  =  let ind = findIndices (==Nothing) aggVals
                                in if length ( ind )== n
                                   then aggVals
                                   else changeWeights ind (vectToMaybeVect weights)

public export
process : Op -> Vect n( Maybe Double) -> Maybe Double
process Min aggVals     = minOfMaybeVect aggVals
process Max aggVals     = maxOfMaybeVect aggVals
process Average aggVals = avgOfMaybeVect aggVals


public export
aggregate : Op -> EvalTree -> EvalTree
aggregate Sum (EvalLeaf x)  = (EvalLeaf x)
aggregate Sum (EvalNode vect{ws} ts r)=let aggTrees = (map (aggregate Sum) ts)
                                           aggVals  = map (getResult) aggTrees
                                           w_vals   = map valWeight ws
                                           changedWeights = arrange w_vals aggVals
                                        in case isNothingVect(changedWeights) of
                                           True  => (EvalNode vect{ws} ts Nothing)
                                           False => let arranged = zipCaseOfNothing aggVals changedWeights
                                                     in (EvalNode vect{ws} ts (Just (sumOfMaybeVect arranged)))
aggregate o (EvalLeaf x) = (EvalLeaf x)
aggregate o (EvalNode vect{ws} ts r)
  = let aggTrees = (map (aggregate o) ts)
        aggVals  = map (getResult) aggTrees
     in (EvalNode vect{ws} ts (process o aggVals ))




min_helper : List Double -> Double -> Double
min_helper [] x = x
min_helper (y :: xs) x = if y < x
                         then min_helper xs y
                         else min_helper xs x

min : List Double -> Double
min [] = 0
min (x :: xs) = min_helper xs x

max_helper : List Double -> Double -> Double
max_helper [] x = x
max_helper (y :: xs) x = if y > x
                         then max_helper xs y
                         else max_helper xs x

max : List Double -> Double
max [] = 0
max (x :: xs) = max_helper xs x


normalize : (Interval l u) -> Maybe Double -> Maybe LeafValue
normalize (MkInterval l u x)  Nothing = Nothing
normalize (MkInterval l u x) (Just input) = let value = ((input-l)/(u-l))
                                             in case choose ( (l<=value) && (value <= u)) of
                                               Left  cons => Just (MkLeaf value (MkInterval l u x) cons)
                                               Right cons => Nothing



normalization : List (Maybe Double) -> List Norm -> List (Maybe LeafValue)
normalization [] norms = []
normalization (x :: xs) [] = []
normalization (x :: xs) ((Single (lower :: (upper :: []))) :: ys)
= case choose (lower <= upper ) of
      Left  s => let interval = (MkInterval lower upper s)
                  in [normalize interval x] ++ normalization xs ys
      Right s => [Nothing] ++ normalization xs ys
normalization (x :: xs) ((Plural zs) :: ys)
= case choose (AggregationLib.min zs <= AggregationLib.max zs) of
      Left  s => let interval = (MkInterval (AggregationLib.min zs) (AggregationLib.max zs) s)
                  in [normalize interval x] ++ normalization xs ys
      Right s => [Nothing] ++ normalization xs ys



aggregator: EvalTree -> List (Maybe Double) ->List Norm -> Op -> Double
aggregator tree inputs norms op= let newInputs = normalization inputs norms
                                  in let newTree   = setLeafs newInputs tree
                                         resultTree= aggregate op newTree
                                      in case getResult resultTree of
                                          Nothing => 0
                                          Just result => result
