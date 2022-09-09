import Test.QuickCheck

import Data.List

----------------------- Exercise 1 - START ---------------------------- 

-- #2 of workshop

-- Functions definition
f1_1:: Int -> Int 
f1_1 n = sum [x^2| x<-[1..n]]

f1_2:: Int -> Int
f1_2 n = n*(n+1)*(2*n+1) `div` 6

-- #2 of workshop
-- Functions definition
f2_1:: Int-> Int
f2_1 n = sum [x^3 | x<-[1..n]]

f2_2:: Int->Int
f2_2 n = ( n*(n+1) `div` 2)^2

-- Helper functions 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

squareRootOfNum :: Int -> Int
squareRootOfNum = floor. sqrt. fromIntegral 


-- Properties definition for workshop exercise #2

-- Property 1: The equality should be tested
prop_equality1:: Int -> Property
prop_equality1 n = (n>=1) ==> (f1_1 n == f1_2 n)

-- Property 2: The sum must be equal of more than n squared 
prop_sumAtLeastN2 :: Int -> Property 
prop_sumAtLeastN2 n = (n>=1) ==> (f1_2 n >=  n ^ 2)

-- Property 3: The difference of two continious outputs n and (n+1) 
--must be n^2 
prop_diff1:: Int -> Property
prop_diff1 n = (n>=1) ==> (f1_2 (n+1) - f1_2 n == (n+1)^2)

-- Properties definition for workshop exercise #3

-- Property 1: The equality should be tested
prop_equality2:: Int -> Property
prop_equality2 n = (n>=1) ==> (f2_1 n == f2_2 n)

-- Property 2: The sum must be equal of more than n^3
prop_sumAtLeastN3 :: Int -> Property
prop_sumAtLeastN3 n = (n>=1) ==> (f2_2 n >= n^3)

-- Property 3: The difference of two continious outputs n and (n+1) 
--must be n^3
prop_diff2:: Int -> Property
prop_diff2 n = (n>=1) ==> (f2_2 (n+1) - f2_2 n == (n+1)^3)

-- Time spent: 1h to get used with QuickCheck

----------------------- Exercise 1 - END ----------------------------