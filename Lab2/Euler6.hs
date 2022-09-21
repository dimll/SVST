module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO
import Data.Function

-- We first calculated n²
square n =n*n
squares n= map (square) [1..n]
-- We then add the list containing n² to find the first part of the difference.
res1 n = sum (squares n)

-- In reverse order we add all numbers together from our natural numbers list. Then we took the square of the result. That made the second part of the result.
sumN n = sum [1..n]
res2 n = square (sumN n)

-- We subtracted the 2 parts to get to the final result.
diff n = res2 n - res1 n

main :: IO () 
main = do 
    print $ diff 100

-- Time Spent : 10 mins