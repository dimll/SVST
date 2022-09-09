import Test.QuickCheck

import Data.List
---------------------------- BONUS 10 START ------------------------------------
-- Euler problem 10
-- https://projecteuler.net/problem=10

-- We are using the previous helper functions to check 
-- for the prime numbers
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primesv2
-- We generate all the prime numbers starting from 2 to 2M
-- Note: 9999991 is the last element 
primesv2 :: [Integer]
primesv2 = 2:filter prime [3..2000000]

-- Calculate the sum of all those numbers
euler10 :: Integer
euler10 = sum primesv2

---------------------------- BONUS 10 END ------------------------------------