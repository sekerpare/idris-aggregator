import Data.Vect

-- HELPERS ------------------------------------------------------------
min_helper : Vect n Double -> Double -> Double
min_helper [] x = x
min_helper (y :: xs) x = if y < x
                         then min_helper xs y
                         else min_helper xs x

min : Vect n Double -> Double
min [] = 0
min (x :: xs) = min_helper xs x

max_helper : Vect n Double -> Double -> Double
max_helper [] x = x
max_helper (y :: xs) x = if y > x
                         then max_helper xs y
                         else max_helper xs x

max : Vect n Double -> Double
max [] = 0
max (x :: xs) = max_helper xs x

--EDWIN --kitaba bak !

empties : Vect m (Vect 0 elem)
empties {m = Z} = []
empties {m = (S k)} = [] :: empties

transposeHelper : (x : Vect m elem) -> (xs_trans : Vect m (Vect len elem)) -> Vect m (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

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

creep : Vect 7 Double
creep = [6.3,1.5, 1.3,0.5, 0.45,0.05,0.09]

sample : Vect 7 Double
sample = [0.00 , 0.39 , 0.17 , 0.35 , 0.30 , 0.84 ,1.00 ]

sample1 : Vect 7 Double
sample1 =[0.00 , 0.30 , 0.36 , 0.29 , 0.40 , 0.71 ,1.00 ]

sample2 : Vect 7 Double
sample2 = [0.00 ,0.77 ,0.80 ,0.93, 0.94, 1.00, 0.99 ]

table: Vect 3 (Vect 7 Double)
table      =[[0.00 ,0.39 ,0.17 ,0.35, 0.30, 0.84, 1.00 ],
             [0.00 ,0.30 ,0.36 ,0.29, 0.40, 0.71, 1.00 ],
             [0.00 ,0.77 ,0.80 ,0.93, 0.94, 1.00, 0.99 ]]

--Step1
--Normalization
normalize: Vect n Double -> Double -> Double -> Vect n Double
normalize [] the_min the_max= []
normalize (x :: xs) the_min the_max= ((x - the_min) / (the_max - the_min ))::(normalize xs the_min the_max)


normalize_table : Vect m (Vect n Double) -> Vect m (Vect n Double)
normalize_table [] = []
normalize_table (x :: xs) = (normalize x (min x) (max x)) :: (normalize_table xs)


--Step2------------------------------------------------------------------------------

mean : Vect n Double -> Double
mean xs{n} = (sum xs)/(cast) n

variance_helper : Vect n Double -> Double -> Double
variance_helper [] the_mean = 0
variance_helper (x :: xs) the_mean = (Prelude.pow (x - the_mean) 2) + variance_helper xs the_mean


variance : Vect n Double -> Double
variance [] = 0
variance (x::xs) = let the_mean = mean (x::xs)
                       t   = variance_helper (x::xs) the_mean
                       len = length (x::xs)
                    in t/(cast) (len-1)


standard_deviation : Vect n Double -> Double
standard_deviation xs = sqrt (variance xs)


objective_weighting: Vect n Double -> Vect n Double
objective_weighting xs = let the_sum = sum xs
                          in map (/ the_sum) xs

calculate_standard_deviation  : Vect m (Vect n Double) -> (Vect m Double)
calculate_standard_deviation  xs = map (standard_deviation) xs


calculate_objective_weighting : Vect m (Vect n Double) -> (Vect m Double)
calculate_objective_weighting xs = objective_weighting (calculate_standard_deviation  xs)


--Step3 ------------------------------------------------------------------------------------------------------------------------------
R_helper : Vect n Double -> Double-> Vect n Double -> Double -> Double
R_helper [] the_mean1 [] the_mean2 = 0
R_helper (z :: zs) the_mean1 (w :: ws) the_mean2 = (z - the_mean1)*( w - the_mean2) + R_helper zs the_mean1 ws the_mean2



R : Vect n Double -> Vect n Double -> Double
R row1 row2 = let upper = R_helper row1 (mean row1) row2 (mean row2)
                  down  = sqrt((variance_helper row1 (mean row1))*(variance_helper row2 (mean row2)))
               in upper/down

calculate_R_helper2 : Vect m (Vect n Double) -> Vect n Double  -> Vect m Double
calculate_R_helper2 ys xs = map (R xs) ys


calculate_R_helper1 : Vect m (Vect n Double) -> Vect m (Vect n Double) -> Vect m (Vect m Double)
calculate_R_helper1 xs ys = map (calculate_R_helper2 ys) xs --(calculate_R_helper2 x ys)::(calculate_R_helper1 xs ys)

--ij
calculate_R : Vect m (Vect n Double) -> Vect m (Vect m Double)
calculate_R [] = []
calculate_R (x :: xs) = calculate_R_helper1 (x :: xs) (x :: xs)

table_R: Vect 3 (Vect 3 Double)
table_R= [[1.0               , 0.9514339618501283, 0.6685155510861626],
          [0.9514339618501283, 1.0               , 0.7145391904371372],
          [0.6685155510861626, 0.7145391904371372, 1.0               ]]


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


{-
calculation_correlation_weighting : Vect n (Vect n Double) -> Vect n Double
calculation_correlation_weighting xs{n} = let list = (take n [0..])
                                              indices = fromList list
                                           in ?heyy0 --map (correlation_of_weighting xs) indices




-------------------------------------------------------------
Wo : Vect 3 Double
Wo =[0.3459082529250316, 0.3128864321680307, 0.3412053149069377]
Wc : Vect 3 Double
Wc=[ 0.287, 0.252, 0.465]


--Step5-----------------------------------------------------------------------------------------------------------
final_Weight_2 : Double -> Double -> Double -> Double
final_Weight_2 w1 w2 lambda = w1*lambda + w2*(1-lambda)

final_Weight_3 : Double -> Double -> Double -> Double -> Double
final_Weight_3 w1 w2 w3 lambda = w1*lambda + w2*(1-lambda)/2 +  w3*(1-lambda)/2
-}
