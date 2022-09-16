module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO
import Data.Function

--This function calculates the numbers that are multiples of 3
multiple3 :: Int -> [Int]
multiple3 n = filter (<n) (map(*3) [1..n])

--This function calculates the numbers that are multiples of 5
multiple5 :: Int -> [Int]
multiple5 n = filter (<n) (map(*5) [1..n])

--This function calculates the numbers that are multiples of 3 or 5. The reason why we used "union" is because we wanted to eliminate duplicates
-- (AUB) = (A)+ (B) - (AnB)
multiple3or5 n = multiple3 n `union` multiple5 n

--We add all the list elements up to find the final result.
res = sum (multiple3or5 1000)

main :: IO () 
main = do 
    print res 