import AggregationLib
import EvalTreeLib
import IntervalLib
import WeightLib
import Data.So
import Data.Vect

w05: Weight
w05= MkWeight 0.05 Oh
w1: Weight
w1= MkWeight 0.1 Oh
w15: Weight
w15= MkWeight 0.15 Oh
w2: Weight
w2= MkWeight 0.2 Oh
w25: Weight
w25= MkWeight 0.25 Oh
w3: Weight
w3= MkWeight  0.3 Oh
w4: Weight
w4= MkWeight 0.4 Oh
w5: Weight
w5= MkWeight 0.5 Oh
w6: Weight
w6= MkWeight 0.6 Oh
w85: Weight
w85= MkWeight 0.85 Oh

v1 : Vect 4 Weight
v1 = [w1 , w2, w5 , w2]


w1Tree : EvalTree
w1Tree = EvalNode (makeVect v1) [EvalLeaf (Just defaultInput)  ,EvalLeaf (Just defaultInput) , EvalLeaf (Just defaultInput)  ,EvalLeaf (Just defaultInput) ] Nothing


w2Tree : EvalTree
w2Tree = EvalNode (makeVect [w3 , w1 , w4,w05,w15]) [EvalLeaf (Just defaultInput) ,EvalLeaf (Just defaultInput) , EvalLeaf (Just defaultInput) , EvalLeaf (Just defaultInput) , EvalLeaf (Just defaultInput) ] Nothing


w3Tree : EvalTree
w3Tree = EvalNode (makeVect [w25, w6,w15] ) [EvalLeaf (Just defaultInput) , EvalLeaf (Just defaultInput) , EvalLeaf (Just defaultInput) ] Nothing

w4Tree : EvalTree
w4Tree = EvalNode (makeVect [w15, w85]) [EvalLeaf (Just defaultInput)  ,  EvalLeaf (Just defaultInput) ] Nothing

w5Tree : EvalTree
w5Tree = EvalNode (makeVect [w5, w1,w4]) [w1Tree,w2Tree,w3Tree] Nothing

root  : EvalTree
root  = EvalNode  (makeVect [w4,w6]) [ w5Tree,w4Tree] Nothing




--NodeValue
inputs : List (Maybe Double)
inputs = [Just 10, Just 20, Just 30, Just 90, Just 30, Just 35, Just 40, Just 50, Just 55 , Just 20 , Just 20, Just 20 ,Just  50, Just 40]

--NodeValue
inputs2 : List (Maybe Double)
inputs2 = [Just 10, Just 20, Nothing, Just 90, Just 30, Just 35, Just 40, Just 50, Just 55 , Just 20 , Just 20, Just 20 ,Just  50, Just 40]

--NodeValue
inputs3 : List (Maybe Double)
inputs3 = [Just 10, Just 20, Nothing, Just 90, Just 30, Just 35, Just 40, Just 50, Just 55 , Just 20 , Just 20, Just 20 ,Just  50, Nothing]

--NodeValue
inputs4 : List (Maybe Double)
inputs4 = [Just 10, Just 20, Nothing, Just 90, Just 30, Just 35, Just 40, Just 50, Just 55 , Just 20 , Just 20, Just 20 ,Nothing , Nothing]

--NodeValue
inputs5 : List (Maybe Double)
inputs5 = [Nothing, Just 20, Nothing, Just 90, Just 30, Just 35, Just 40, Just 50, Just 55 , Just 20 , Just 20, Just 20 ,Nothing , Nothing]

inputs6 : List (Maybe Double)
inputs6 = [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing , Nothing, Nothing ,Nothing , Nothing]



norms : List Norm
norms = [Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100],Single [0,100]]
