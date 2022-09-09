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

-- Time spent: 15mins

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
-- Math source: https://math.stackexchange.com/questions/41623/is-that-true-that-all-the-prime-numbers-are-of-the-form-6m-pm-1

-- Helper function 
testEqAfterModBy6 :: Integer -> Bool
testEqAfterModBy6 n = (n `mod` 6 == 1 || n `mod` 6 ==5) || (n<=5) 

-- Test function
emirp :: Bool
emirp = length ( filter testEqAfterModBy6 reversibleStream ) == length reversibleStream

-- Time spent: 1h
----------------------- Exercise 4 - END ----------------------------
----------------------- Exercise 5 - START ----------------------------
-- Time spent: 80 minutes

-- Helpers
-- prime :: Integer -> Bool
-- prime n = n > 1 && all (\x -> rem n x /= 0) xs
--   where
--     xs = takeWhile (\y -> y ^ 2 <= n) primes

-- primes :: [Integer]
-- primes = 2 : filter prime [3 .. 10000]

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




----------------------- Exercise 5 - END ----------------------------
----------------------- Exercise 6 - START ----------------------------

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
----------------------- Exercise 7 - START ----------------------------

-- -- Source: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
-- getDigits :: Integer -> [Integer]
-- getDigits 0 = []
-- getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]

-- getMiddleResult :: [Integer] -> [Integer]
-- getMiddleResult l = zipWith (*) l (take (n-1)  $ cycle[1,2])
--   where n = length l

-- uniteAndSum :: Integer -> Integer
-- uniteAndSum k 
--   | k <= 9 = k
--   |otherwise = sum (getDigits k)

-- sumDigits :: [Integer] -> Integer 
-- sumDigits l = sum (map uniteAndSum l) 

-- checkDigit :: Integer -> Integer
-- checkDigit k = 10 - (k `mod` 10)

-- originalCheckDigit:: Integer -> Integer
-- originalCheckDigit k = last (getDigits k)

-- checkDigitCalculation:: Integer -> Integer
-- checkDigitCalculation k = checkDigit( sumDigits( getMiddleResult (take (n-1) (getDigits k)))) 
--   where n = length (getDigits k) 

-- luhn ::  Integer -> Bool 
-- luhn k
--   | originalCheckDigit k == checkDigitCalculation k = True
--   | otherwise = False   

--------------------------- EXERCISE 7 CORRECT------------------------------------- 
luhn :: Integer -> Bool
luhn n = compareChecksums $ convertIntToIntArray n

-- Source: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
getDigits :: Integral x => x -> [x]
getDigits 0 = []
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]

-- Compare the payload checksum to the given checksum.
compareChecksums :: [Int] -> Bool
compareChecksums a = getPayloadChecksum (take (length a - 1) a) == last a

-- Helper function to cut down on redundant code
convertIntToIntArray :: Integer -> [Int]
convertIntToIntArray n = map (read . (: "")) (show n)

-- Calculate the checksum of the sum of the payload after appropriate calculations.
getPayloadChecksum :: [Int] -> Int
getPayloadChecksum a = (10 - getPayloadSum a `mod` 10) `mod` 10

-- The sum of the entire payload (complete Int - last digit)
getPayloadSum :: [Int] -> Int
getPayloadSum a = sum (sumIntsOver10 $ makeWeightedProducts a)

-- Map over array and do the correct calculations for each digit.
sumIntsOver10 :: [Int] -> [Int]
sumIntsOver10 = map digitSummation

-- If n > 9 return split the Int into a [Int] and return return the sum.
digitSummation :: Int -> Int
digitSummation n
  | n <= 9 = n
  | otherwise = sum $ convertIntToIntArray (toInteger n)

-- Get the product of the weights mulitplied by the input array
makeWeightedProducts :: [Int] -> [Int]
makeWeightedProducts a = zipWith (*) a (makeWeights a)

-- Make an array of weights where every other digit is 2 starting from the right
makeWeights :: [Int] -> [Int]
makeWeights a = reverse $ take (length a) (cycle [2, 1])

-- Check if first two digits are 34 or 37, length is 15 and passes Luhn.
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n && length (show n) == 15 && (take 2 (getDigits n) == [3, 4] || take 2 (getDigits n) == [3, 7])

-- Check if first digit is between 51 and 55 or 2221 and 2720, length is 16 and passes Luhn.
isMaster :: Integer -> Bool
isMaster n = luhn n && length (show n) == 16 && ((read (take 2 (show n)) >= 51 && read (take 2 (show n)) <= 55) || (read (take 2 (show n)) >= 2221 && read (take 2 (show n)) <= 2720))

-- Check if first digit is 4, length is 16 and passes Luhn.
isVisa :: Integer -> Bool
isVisa n = luhn n && (length (show n) == 16 || length (show n) == 13) && take 1 (getDigits n) == [4]

-- We are testing our functions by performing the tests presented below

-- Valid Lugn Numbers
-- Source: https://www.dcode.fr/luhn-algorithm
validLuhnNumbers :: [Integer]
validLuhnNumbers = [1412, 15550684, 3845098536, 095534719570, 890305230745842, 1172930479145920, 333875529230383809] 

-- A valid American Express Card, Master Card, or Visa Card number must first satisfy the 
-- luhn algorithm. Valid test cases for each type of cards are presented below
-- Source: https://www.freeformatter.com/credit-card-number-generator-validator.html
americanExpValid, masterCardValid,visaValid :: [Integer]
americanExpValid = [378282246310005,371449635398431,378734493671000]
masterCardValid = [5307149794595276, 5302127968008420]
visaValid = [4539053627938021, 4916291466783958]

-- Helper function for testing. Must return true if all the data is valid
-- Takes a list of numbers to check and if they pass all tests it returns true, otherwise false
-- testingList: The list to be tested by algorithms 
-- Example: testingValidity isMaster masterCardValid
testingValidity :: (Integer -> Bool) -> [Integer] -> Bool
testingValidity validityFuncton testingList = length (filter (==True) (map validityFuncton testingList)) == length testingList

-- Time spent: 3h
--------------------------- EXERCISE 7 END------------------------------------- 
---------------------------- BONUS 9 START ------------------------------------
-- Euler problem 9
-- https://projecteuler.net/problem=9

-- The maximum possible number for b is 499 since all numbers
-- are natural and a<b<c. So lets try b from 2 to 499
-- c <- [1..998]
bPossibleList :: [Integer]
bPossibleList = [2..499]

-- Also since a+b+c=1000 => c = 1000-a-b should be a constant
-- Finally from the PT: a^2+b^2 must be equal to c^2

-- Using haskell list comprehension
euler9  :: Integer
euler9 = head [a*b*c | b<-bPossibleList, a<-[1..b], let c = 1000-a-b, a^2+b^2 == c^2]

-- (Better) intresting solution: https://www.xarg.org/puzzle/project-euler/problem-9/
---------------------------- BONUS 9 END ------------------------------------

---------------------------- BONUS 10 START ------------------------------------
-- Euler problem 10
-- https://projecteuler.net/problem=10

-- We are using the previous helper functions to check 
-- for the prime numbers

primev2 :: Integer -> Bool
primev2 n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primesv2

-- We generate all the prime numbers starting from 2 to 2M
-- Note: 9999991 is the last element 
primesv2 :: [Integer]
primesv2 = 2:filter primev2 [3..2000000]

-- Calculate the sum of all those numbers
euler10 :: Integer
euler10 = sum primesv2

---------------------------- BONUS 10 END ------------------------------------
-- MAIN FUNCTION---
main :: IO ()
main = do
    let l1 = [1,2,3,4,5,6,7,8,9,10]
    --putStrLn "heyy!"
    --print (luhnTestingFunction validLuhnNumbers)
    print (euler10)
    --print (modBy6E1 primes) 
    --quickCheck prop_test 
   
    
