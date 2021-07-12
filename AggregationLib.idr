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
sumMaybeVect :Vect m (Maybe Double) ->Double
sumMaybeVect [] = 0
sumMaybeVect (Nothing  :: xs) = sumMaybeVect xs
sumMaybeVect ((Just x) :: xs) = x + sumMaybeVect xs




public export
totalExeptGiven:Fin n-> Vect n (Maybe Double) ->Double
totalExeptGiven FZ [] impossible
totalExeptGiven (FS _) [] impossible
totalExeptGiven ind (weight::weights) = let dropped = deleteAt ind (weight::weights)
                                         in sumMaybeVect dropped


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
minMaybeVectHelper:Vect m (Maybe Double)->Maybe Double->Maybe Double
minMaybeVectHelper [] x = x
minMaybeVectHelper (Nothing :: xs)  Nothing  = minMaybeVectHelper xs Nothing
minMaybeVectHelper ((Just x) :: xs) Nothing  = minMaybeVectHelper xs (Just x)
minMaybeVectHelper (Nothing :: xs)  (Just x) = minMaybeVectHelper xs (Just x)
minMaybeVectHelper ((Just y) :: xs) (Just x) = case x<y of
                                                  True  => minMaybeVectHelper xs (Just x)
                                                  False => minMaybeVectHelper xs (Just y)


public export
minMaybeVect :Vect m (Maybe Double) -> Maybe Double
minMaybeVect [] = Nothing
minMaybeVect (x :: xs) = minMaybeVectHelper xs x



public export
maxMaybeVectHelper:Vect m (Maybe Double)->Maybe Double->Maybe Double
maxMaybeVectHelper [] x = x
maxMaybeVectHelper (Nothing :: xs)  Nothing  = maxMaybeVectHelper xs Nothing
maxMaybeVectHelper ((Just x) :: xs) Nothing  = maxMaybeVectHelper xs (Just x)
maxMaybeVectHelper (Nothing :: xs)  (Just x) = maxMaybeVectHelper xs (Just x)
maxMaybeVectHelper ((Just y) :: xs) (Just x) = case x>y of
                                                  True  => maxMaybeVectHelper xs (Just x)
                                                  False => maxMaybeVectHelper xs (Just y)


public export
maxMaybeVect :Vect m (Maybe Double) -> Maybe Double
maxMaybeVect [] = Nothing
maxMaybeVect (x :: xs) = maxMaybeVectHelper xs x

public export
lenghtMaybeVect : Vect m (Maybe elem) -> Nat
lenghtMaybeVect [] = 0
lenghtMaybeVect (Nothing :: xs)  = lenghtMaybeVect xs
lenghtMaybeVect ((Just x) :: xs) = 1 + lenghtMaybeVect xs

public export
avgMaybeVect :Vect m (Maybe Double) -> Maybe Double
avgMaybeVect xs{m} = case isNothingVect xs of
                        True => Nothing
                        False=> let s = sumMaybeVect xs
                                    l = lenghtMaybeVect xs
                                 in Just (s/(cast)l)




public export
arrange : Vect n Double -> Vect n( Maybe Double)  ->Vect n( Maybe Double)
arrange weights{n} aggVals  =  let ind = findIndices (==Nothing) aggVals
                                in if length ( ind )== n
                                   then aggVals
                                   else changeWeights ind (vectToMaybeVect weights)

public export
process : Op -> Vect n( Maybe Double) -> Maybe Double
process Min aggVals     = minMaybeVect aggVals
process Max aggVals     = maxMaybeVect aggVals
process Average aggVals = avgMaybeVect aggVals


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
                                                     in (EvalNode vect{ws} ts (Just (sumMaybeVect arranged)))
aggregate o (EvalLeaf x) = (EvalLeaf x)
aggregate o (EvalNode vect{ws} ts r)
  = let aggTrees = (map (aggregate o) ts)
        aggVals  = map (getResult) aggTrees
     in (EvalNode vect{ws} ts (process o aggVals ))



public export
min_helper : List Double -> Double -> Double
min_helper [] x = x
min_helper (y :: xs) x = if y < x
                         then min_helper xs y
                         else min_helper xs x

public export
min : List Double -> Double
min [] = 0
min (x :: xs) = min_helper xs x

public export
max_helper : List Double -> Double -> Double
max_helper [] x = x
max_helper (y :: xs) x = if y > x
                         then max_helper xs y
                         else max_helper xs x

public export
max : List Double -> Double
max [] = 0
max (x :: xs) = max_helper xs x


public export
normalize : (Interval l u) -> Norm -> Maybe Double -> Maybe LeafValue
normalize (MkInterval l u x) norm Nothing = Nothing
normalize (MkInterval l u x) (Single (nl :: (nu :: []))) (Just input)
= let value = ((input-nl)/(nu-nl))
   in case choose ( (l<=value) && (value <= u)) of
       Left  cons => Just (MkLeaf value (MkInterval l u x) cons)
       Right cons => Nothing
normalize (MkInterval l u x) (Plural zs) (Just input)
= let nl=AggregationLib.min zs
      nu=AggregationLib.max zs
      value = ((input-nl)/(nu-nl))
   in case choose ( (l<=value) && (value <= u)) of
       Left  cons => Just (MkLeaf value (MkInterval l u x) cons)
       Right cons => Nothing




public export
normalization : List (Maybe Double) -> List Norm ->(Interval l u)-> List (Maybe LeafValue)
normalization [] norms interval= []
normalization (x :: xs) [] interval= []
normalization (x :: xs) (norm::norms) interval
= [normalize interval norm x] ++ normalization xs norms interval



public export
aggregator: EvalTree -> List (Maybe Double) -> List Norm -> Op ->(Interval l u) -> Double
aggregator tree inputs norms op interval= let newInputs = normalization inputs norms interval
                                           in let newTree   = setLeafs newInputs tree
                                                  resultTree= aggregate op newTree
                                               in case getResult resultTree of
                                                 Nothing => 0
                                                 Just result => result
