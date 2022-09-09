module Main where

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
prop_equality1:: Int -> Bool 
prop_equality1 n = (n>=1) --> (f1_1 n == f1_2 n)

-- Property 2: The sum must be equal of more than n squared 
prop_sumAtLeastN2 :: Int -> Bool 
prop_sumAtLeastN2 n = (n>=1) --> (f1_2 n >=  n ^ 2)

-- Property 3: The difference of two continious outputs n and (n+1) 
--must be n^2 
prop_diff1:: Int -> Bool
prop_diff1 n = (n>=1) --> (f1_2 (n+1) - f1_2 n == (n+1)^2)

-- Properties definition for workshop exercise #3

-- Property 1: The equality should be tested
prop_equality2:: Int -> Bool 
prop_equality2 n = (n>=1) --> (f2_1 n == f2_2 n)

-- Property 2: The sum must be equal of more than n^3
prop_sumAtLeastN3 :: Int -> Bool 
prop_sumAtLeastN3 n = (n>=1) --> (f2_2 n >= n^3)

-- Property 3: The difference of two continious outputs n and (n+1) 
--must be n^3
prop_diff2:: Int -> Bool
prop_diff2 n = (n>=1) --> (f2_2 (n+1) - f2_2 n == (n+1)^3)

----------------------- Exercise 1 - END ----------------------------

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

----------------------- Exercise 2 - END -----------------------------
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

----------------------- Exercise 3 - END ----------------------------
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
-- Math source: +++++++ !!!!!!!!!!!!! TO DO 

-- Helper function 
testEqAfterModBy6 :: Integer -> Bool
testEqAfterModBy6 n = (n `mod` 6 == 1 || n `mod` 6 ==5) || (n<=5) 

-- Test function
emirp :: Bool
emirp = length ( filter testEqAfterModBy6 reversibleStream ) == length reversibleStream
----------------------- Exercise 4 - END ----------------------------
----------------------- Exercise 5 - START ----------------------------

-- Ahmud solution!

-- Function helper (index, )
-- needs primes list
consPrimeSum:: Int -> Integer
consPrimeSum index = sum (drop index . take (index+101) $ primes) 


----------------------- Exercise 5 - END ----------------------------
----------------------- Exercise 6 - START ----------------------------
multPrimes:: Int -> Integer
multPrimes x = product (take x primes)

tList :: [Int]
tList = [2..100]

pList :: [Integer]
pList = [multPrimes x | x<- [2..10]]

pListP1 :: [Integer]
pListP1 = map (+1) pList  

counterEx :: [Integer]
counterEx = filter (not .prime)  pListP1
----------------------- Exercise 6 - END ----------------------------
----------------------- Exercise 7 - START ----------------------------

-- Source: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
getDigits :: Integer -> [Integer]
getDigits 0 = []
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]

getMiddleResult :: [Integer] -> [Integer]
getMiddleResult l = zipWith (*) l (take (length l - 1) $ cycle[1,2])

uniteAndSum :: Integer -> Integer
uniteAndSum k 
  | k <= 9 = k
  |otherwise = sum (getDigits k)

sumDigits :: [Integer] -> Integer 
sumDigits l = sum (map uniteAndSum l) 

checkDigit :: Integer -> Integer
checkDigit k = 10 - (k `mod` 10)

originalCheckDigit:: Integer -> Integer
originalCheckDigit k = last (getDigits k)

checkDigitCalculation:: Integer -> Integer
checkDigitCalculation k = checkDigit( sumDigits( getMiddleResult (take 15 (getDigits k)))) 

luhn ::  Integer -> Bool 
luhn k
  | originalCheckDigit k == checkDigitCalculation k = True
  | otherwise = False   

-- MAIN FUNCTION---
main :: IO ()
main = do
    let l1 = [1,2,3,4,5,6,7,8,9,10]
    --putStrLn "heyy!"
    print (uniteAndSum 8)
    --print (sumDigits( getMiddleResult( getDigits 1234)))
    --print (modBy6E1 primes) 
    --quickCheck prop_test 
   
    
