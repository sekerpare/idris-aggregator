import AggregationLib
import EvalTreeLib
import IntervalLib
import WeightLib
import Data.So
import Data.Vect



w1: Weight
w1= MkWeight 0.32 Oh
w2: Weight
w2 = MkWeight 0.28 Oh
w3: Weight
w3= MkWeight 0.40 Oh

v : Vect 3 Weight
v = [w1 , w2, w3 ]


Tree : EvalTree
Tree = EvalNode (makeVect v) [EvalLeaf (Just defaultInput)  ,EvalLeaf (Just defaultInput), EvalLeaf (Just defaultInput) ] Nothing


--NodeValue
inputs : List (Maybe Double)
inputs = [Just 45, Just 302, Just 0.05]

--NodeValue
inputs2 : List (Maybe Double)
inputs2 = [Nothing, Nothing, Nothing]

--NodeValue
inputs3 : List (Maybe Double)
inputs3 = [Just 45, Just 516, Nothing]



norms : List Norm
norms = [Plural [45,141,88,132,118,252,292],Plural [302,366,380,364,387,455,516],Plural [6.3,1.5,1.3,0.5,0.45,0.05,0.09]]
