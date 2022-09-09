import Test.QuickCheck

import Data.List
----------------------- Exercise 4 - START ----------------------------

-- Helper functions
reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
-- We set a bound (of 10000) to the list of primes
-- to be in terms with the assignment hypothesis
primes :: [Integer]
primes = 2:filter prime [3..10000]

reversibleStream :: [Integer]
reversibleStream = filter (prime . reversal) primes

-- To test this function we are defining the following property:
-- Two test if a (more than 1 digit ) number is prime and its 
-- reversable is prime as well the modulo with 6 must be equal to 1
-- or 5. This is a property of reversed primes
-- Math source: 

-- Helper function 
testEqAfterModBy6 :: Integer -> Bool
testEqAfterModBy6 n = (n `mod` 6 == 1 || n `mod` 6 ==5) || (n<=5) 

-- Test function
emirp :: Bool
emirp = length ( filter testEqAfterModBy6 reversibleStream ) == length reversibleStream

-- Time spent: 1h
----------------------- Exercise 4 - END ----------------------------