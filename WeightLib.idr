module WeightLib

import IntervalLib
import Data.Vect
import Data.So

%default total

public export
isSumOne : Vect n Double  -> Bool
isSumOne x = if sum x ==1
             then True
             else False


public export
data Weight: Type where
    MkWeight : (value:Double)
    -> So ( (0<=value) && (value <=1)) -> Weight


public export
valWeight : Weight-> Double
valWeight (MkWeight value x) = value


public export
data WeightVect   : (xs : Vect n Weight)  ->Type where
    MkWeightVect  : (xs : Vect n Weight)  ->So( isSumOne (map WeightLib.valWeight xs))
    -> WeightVect xs


public export
makeVect: (v:Vect n Weight) -> WeightVect v
makeVect ws = case choose ( isSumOne (map (WeightLib.valWeight) ws)) of
                Left  s => (MkWeightVect  ws s)
                Right s => ?k--(MkWeightVect  ws s)
