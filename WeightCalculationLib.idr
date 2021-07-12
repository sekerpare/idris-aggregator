import Data.Vect
import AggregationLib
data  LargerIsBetter = True | False

-- HELPERS ------------------------------------------------------------
--transpose_mat directly taken from:
--[1] Brady, E. (2017). 3.3.2 Transposing a matrix. In Type-driven development with Idris. essay, Manning Publications Co. 

--[1] 
empties : Vect m (Vect 0 elem)
empties {m = Z} = []
empties {m = (S k)} = [] :: empties

--[1] 
transposeHelper : (x : Vect m elem) -> (xs_trans : Vect m (Vect len elem)) -> Vect m (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

--[1] 
transpose_mat : Vect n (Vect m elem) -> Vect m (Vect n elem)
transpose_mat [] = empties
transpose_mat (x :: xs)
  = let xs_trans = transpose_mat xs in
        transposeHelper x  xs_trans


------------------------------------------------------------------------
-- Example Data
table_raw : Vect 7 (Vect 3 Double)
table_raw=[[45 , 302, 6.3 ],
           [141, 366, 1.5 ],
           [88 , 380, 1.3 ],
           [132, 364, 0.5 ],
           [118, 387, 0.45],
           [252, 455, 0.05],
           [292, 516, 0.09]]

table_1  : Vect 3 (Vect 7 Double)
table_1  =[[45, 141, 88, 132, 118, 252, 292 ],
           [302,366, 380,364, 387, 455, 516 ],
           [6.3,1.5, 1.3,0.5, 0.45,0.05,0.09]]

table_m  : Vect 3 (Vect 7 (Maybe Double))
table_m  =[[Just 45 , Just 141, Just  88 , Just 132, Just  118 , Just  252, Just  292 ],
           [Just 302, Just 366, Just  380, Just 364, Just  387 , Just  455, Just  516 ],
           [Just 6.3, Just 1.5, Just  1.3, Just 0.5, Just  0.45, Just 0.05, Just 0.09 ]]

deneme:Vect 7 (Maybe Double)
deneme= [Just 45 , Just 141, Just  88 , Just 132, Just  118 , Just  252, Just  292 ]

larger : Vect 3 LargerIsBetter
larger =[True, True, False]

creep : Vect 7 Double
creep = [6.3,1.5, 1.3,0.5, 0.45,0.05,0.09]

sample : Vect 7 Double
sample = [0.00 , 0.39 , 0.17 , 0.35 , 0.30 , 0.84 ,1.00 ]

sample1 : Vect 7 Double
sample1 =[0.00 , 0.30 , 0.36 , 0.29 , 0.40 , 0.71 ,1.00 ]

sample2 : Vect 7 Double
sample2 = [0.00 ,0.77 ,0.80 ,0.93, 0.94, 1.00, 0.99 ]

table: Vect 3 (Vect 7 (Maybe Double))
table      =[[Just 0.0,
  Just 0.38866396761133604,
  Just 0.17408906882591094,
  Just 0.3522267206477733,
  Just 0.29554655870445345,
  Just 0.8380566801619433,
  Just 1.0],
 [Just 0.0,
  Just 0.29906542056074764,
  Just 0.3644859813084112,
  Just 0.2897196261682243,
  Just 0.397196261682243,
  Just 0.7149532710280374,
  Just 1.0],
 [Just 0.0,
  Just 0.768,
  Just 0.8,
  Just 0.9279999999999999,
  Just 0.9359999999999999,
  Just 1.0,
  Just 0.9936]]


--Step1
--Normalization
normalize: Vect n (Maybe Double) -> Maybe Double -> Maybe Double -> Vect n (Maybe Double)
normalize [] lower upper = []
normalize (x :: xs)  Nothing Nothing  = Nothing::(normalize xs Nothing Nothing )
normalize (x :: xs)  Nothing (Just u) = Nothing::(normalize xs Nothing (Just u))
normalize (x :: xs) (Just l) Nothing  = Nothing::(normalize xs (Just l) Nothing)
normalize (Nothing :: xs) (Just l) (Just u)  = Nothing::(normalize xs (Just l) (Just u))
normalize ((Just x) :: xs) (Just l) (Just u) = Just ((x - l) / (u - l ))::(normalize xs (Just l) (Just u))



normalize_table : Vect m (Vect n (Maybe Double))->Vect m LargerIsBetter -> Vect m (Vect n (Maybe Double))
normalize_table [] [] = []
normalize_table (x :: xs) (True :: bs)
  = (normalize x (minMaybeVect x) (maxMaybeVect x))
    :: (normalize_table xs bs)
normalize_table (x :: xs) (False :: bs)
  = (normalize x (maxMaybeVect x) (minMaybeVect x))
    :: (normalize_table xs bs)


--Step2------------------------------------------------------------------------------


mean : Vect n (Maybe Double) -> Double
mean xs = (sumMaybeVect xs)/(cast) (lenghtMaybeVect xs )


variance_helper : Vect n (Maybe Double) -> Double -> Double
variance_helper [] the_mean = 0
variance_helper (Nothing  :: xs) the_mean = variance_helper xs the_mean
variance_helper ((Just x) :: xs) the_mean = (Prelude.pow (x - the_mean) 2) + variance_helper xs the_mean


variance : Vect n (Maybe Double) -> Double
variance [] = 0
variance (Nothing::xs)  = variance xs
variance ((Just x)::xs) = let the_mean = mean ((Just x)::xs)
                              t   = variance_helper ((Just x)::xs) the_mean
                              len = lenghtMaybeVect ((Just x)::xs)
                           in t/(cast) (len-1)


standard_deviation : Vect n (Maybe Double) -> Double
standard_deviation xs = sqrt (variance xs)


objective_weighting: Vect n Double -> Vect n Double
objective_weighting xs = let the_sum = sum xs
                          in map (/ the_sum) xs


calculate_standard_deviation  : Vect m (Vect n (Maybe Double)) -> (Vect m Double)
calculate_standard_deviation  xs = map (standard_deviation) xs


calculate_objective_weighting : Vect m (Vect n (Maybe Double)) -> (Vect m Double)
calculate_objective_weighting xs = objective_weighting (calculate_standard_deviation  xs)


--Step3 ------------------------------------------------------------------------------------------------------------------------------
R_helper : Vect n (Maybe Double) -> Double-> Vect n (Maybe Double) -> Double -> Double
R_helper [] the_mean1 [] the_mean2 = 0
R_helper  (Nothing :: zs) the_mean1 (w :: ws) the_mean2        = R_helper zs the_mean1 ws the_mean2
R_helper ((Just z) :: zs) the_mean1 (Nothing :: ws) the_mean2  = R_helper zs the_mean1 ws the_mean2
R_helper ((Just z) :: zs) the_mean1 ((Just w) :: ws) the_mean2 = (z - the_mean1)*( w - the_mean2) + R_helper zs the_mean1 ws the_mean2



R : Vect n (Maybe Double) -> Vect n (Maybe Double) -> Double
R row1 row2 = let num = R_helper row1 (mean row1) row2 (mean row2)
                  den  = sqrt((variance_helper row1 (mean row1))*(variance_helper row2 (mean row2)))
               in num/den

calculate_R_helper2 : Vect m (Vect n (Maybe Double)) -> Vect n (Maybe Double)  -> Vect m Double
calculate_R_helper2 ys xs = map (R xs) ys


calculate_R_helper1 : Vect m (Vect n (Maybe Double)) -> Vect m (Vect n (Maybe Double)) -> Vect m (Vect m Double)
calculate_R_helper1 xs ys = map (calculate_R_helper2 ys) xs --(calculate_R_helper2 x ys)::(calculate_R_helper1 xs ys)

--ij
calculate_R : Vect m (Vect n (Maybe Double)) -> Vect m (Vect m Double)
calculate_R [] = []
calculate_R (x :: xs) = calculate_R_helper1 (x :: xs) (x :: xs)

table_R: Vect 3 (Vect 3 Double)
table_R= [[1.0, 0.951824747955148, 0.6726760793344575],
 [0.951824747955148, 1.0, 0.718280817334199],
 [0.6726760793344575, 0.718280817334199, 1.0]]


minus1 : Double -> Double
minus1 x = 1-x


corr_helper : Vect n Double -> Double
corr_helper xs = sum (map (minus1) xs)


correlation_of_weighting : Vect n (Vect n Double) -> Integer -> Double
correlation_of_weighting []{n = Z} x = 0
correlation_of_weighting (x :: xs){n = (S len)} ind = let newInd = restrict len ind
                                                          row = index newInd (x :: xs)
                                                          up  = corr_helper row
                                                          down= sum (map (corr_helper) (x :: xs))
                                                       in up/down



calculation_correlation_weighting : Vect n (Vect n Double) -> Vect n Double
calculation_correlation_weighting xs{n} = let list = (take n [0..])
                                              indices = fromList list
                                           in ?heyy0 --map (correlation_of_weighting xs) indices




-------------------------------------------------------------
Wo : Vect 3 Double
Wo =[0.3459082529250316 , 0.3128864321680307, 0.3412053149069377]
Wc : Vect 3 Double
Wc= [0.28567307169582673, 0.250977800613785 , 0.4633491276903883]


--Step5-----------------------------------------------------------------------------------------------------------
final_weight_2 : Double -> Double -> Double -> Double
final_weight_2 lambda w1 w2  = w1*lambda + w2*(1-lambda)

final_weight_3 : Double -> Double -> Double -> Double -> Double
final_weight_3 w1 w2 w3 lambda = w1*lambda + w2*(1-lambda)/2 +  w3*(1-lambda)/2


calculate_final_weight2 : (Vect n Double) -> (Vect n Double) -> Double -> (Vect n Double)
calculate_final_weight2  wt1 wt2 lambda = zipWith (final_weight_2 lambda)  wt1 wt2

calculate_final_weight3 : (Vect n Double)->(Vect n Double) -> (Vect n Double) -> Double -> (Vect n Double)
calculate_final_weight3  wt1 wt2 wt3 lambda = zipWith3 (final_weight_3 lambda)  wt1 wt2 wt3
