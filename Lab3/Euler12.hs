-- Time spent: 30 mins

import Data.List
import Data.Char
import Test.QuickCheck

-- Calculates the triangular number P based of the position n (input)
-- For example P7 = 28
triangularNumber :: Int -> Int
triangularNumber n = sum [1..n] 

-- Calculates the triangular numbers list from the first position
-- to positition input number n 
triangularNumberList :: [Int]
triangularNumberList = map triangularNumber [1..]

-- Outputs the list of divisors of input number n 
totalDivisors :: Int -> [Int]
totalDivisors n = [ x | x<-[1..n], n `mod` x == 0]

-- Calculates the number just before the condition 
-- length  (totalDivisors x)  < (n+1)
-- is equal to False
calculateLastNum :: Int -> Int
calculateLastNum n = last $ takeWhile (\x -> length  (totalDivisors x)  < (n+1)) triangularNumberList

-- Uses the number of the previous function as input in order to calculate 
-- the next triangular number which is the desired output. First it finds the 
-- position of the triangular number and increases it by 1. Then it calculates 
-- the triangular number of the next position. 
pickFinalNumber :: Int -> Int
pickFinalNumber n =  triangularNumber ((triangularToNum n) + 1)   

-- Final function: combines the above to find the first triangular number that has 
-- over five hundred (500) divisors
euler12 :: Int
euler12 = pickFinalNumber $ calculateLastNum 500 

-- Helper functions

-- Helper function 1: Calculate the sqrt with Int data type format for both input
-- and output
-- Source: https://stackoverflow.com/questions/6695267/get-sqrt-from-int-in-haskell
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- Helper function 2: Calculate the position P of a triangular number n 
-- For example the position P of triangular number 28 is 7
-- equation source: https://math.stackexchange.com/questions/698961/finding-the-triangular-root-of-a-number
triangularToNum :: Int -> Int 
triangularToNum n = isqrt $ 2*n

main :: IO ()
main = do 
    -- Answer: 76576500
    print (euler12)

-- We must note that the above implementation is very time consuming 
-- in most modern CPUs the execution will take more than 10min.