import Test.QuickCheck

import Data.List
----------------------- Exercise 5 - START ----------------------------

-- Helpers
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3 .. 10000]

-- Solution
-- Helper function 1:
-- Get a subset of 101 elements from the primes. From index x to x + 101
getPrimes :: Int -> [Integer]
getPrimes x = drop x . take (x + 101) $ primes

-- Helper function 2:
-- I assume that the first prime of the sum of 101 consecutive primes will be in the first 10 000 sums.
-- Therefore, I create a list of the first 100 summations, where each summation is derived from a different list of 101 consecutive primes.
first100Consecutive101PrimeSums :: [Integer]
first100Consecutive101PrimeSums = [sum (getPrimes x) | x <- [0 .. 100]]

-- Filter out all summations that are not primes, return the head as this should be the smallest prime number that is a sum of 101 consecutive primes.
consecutive101Prime :: Integer
consecutive101Prime = head (filter prime first100Consecutive101PrimeSums)

-- Time spent: 80 minutes
----------------------- Exercise 5 - END ----------------------------
