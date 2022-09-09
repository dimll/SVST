import Test.QuickCheck

import Data.List
----------------------- Exercise 6 - START ----------------------------

-- Helpers
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3 .. 10000]

-- We followed a moving window approach 
multPrimes:: Int -> Integer
multPrimes x = product (take x primes)

-- Starting from n=2 we go to n=10, with a step of one
pList :: [Integer]
pList = [multPrimes x | x<- [2..10]]

-- We add +1 to every element of the previous List 
pListP1 :: [Integer]
pListP1 = map (+1) pList  

-- We form counter examples - they exist hehe
counterEx :: [Integer]
counterEx = filter (not .prime)  pListP1

-- Time spent: 50mins

----------------------- Exercise 6 - END ----------------------------