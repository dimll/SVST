import Test.QuickCheck

import Data.List
----------------------- Exercise 2 - START ----------------------------

-- Property  
prop_eqElements :: Int -> Property
prop_eqElements n = n>0 && n<15 ==> length (subsequences [1..n]) == 2^n
-- The above property is hard to test because QuickCheck can generate 
-- a relative big number and in combination with the complexity of the 
-- subsequences function this could lead to the execution being extemely 
-- time consuming. That is why we are trying to "minimize" the test inputs

-- We are testing whether subsequences satisfies a part of its specification
-- since we are unable to check the whole space of possible inputs

-- Time spent: 10 mins
----------------------- Exercise 2 - END -----------------------------
