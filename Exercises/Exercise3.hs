import Test.QuickCheck

import Data.List
----------------------- Exercise 3 - START ----------------------------
-- Helper function
factorialOfInt :: Int->Int
factorialOfInt 0 = 1
factorialOfInt n = n * factorialOfInt (n - 1)

prop_test :: Int -> Property
prop_test n = (n>0 && n<=10) ==> length (permutations [1..n]) == factorialOfInt n 
-- It is hard to test for the exact same reason of the previous exercise property 
-- Moreover the factorial is a recursive function that takes a lot of time as well
-- Factorials also get bigger really fast (faster than exponential functions) so we 
-- also need to be strict with the maximum length because it need to be within 
-- the Int data type boundaries

-- Once again, we are testing whether subsequences satisfies a part of its specification
-- since we are unable to check the whole space of possible inputs

-- Time spent: 15mins

----------------------- Exercise 3 - END ----------------------------