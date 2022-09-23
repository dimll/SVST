module Main where

import Test.QuickCheck
import Data.List
import Data.Char
import System.Random
import System.IO
import SetOrd
import Data.Function
import Lecture3

---------------------------Ex 43: Euler: -----------------------------

--isPrime' function checks if a given number is prime or not. This function is taken from the 
-- lecture notes of week1:

isPrime' :: Integer -> Bool
isPrime' n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]

-- This function turns a list of integers to an actual integer. 
-- For ex: if you input [1,2,3], it will return you 123. We used this function in order to turn
-- our permutations to actuall numbers.
joiner :: [Integer] -> Integer
joiner = read . concatMap show

--This function computes the all possible permutations of a list.
--For ex: if you input [1,2,3] it will output [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,2,1],[3,1,2]]
permut :: Eq a => [a] -> [[a]]
permut [] = [[]]
permut as = do 
    a<- as
    let l = delete a as
    ls <- permut l
    return $ a : ls

--this function calls the sub functions d234,d345,d456,... which are property functions that hold information 
-- on checking whether a given number is a pandigital number or not. It combines all those properties in a single list in order to check if they all satisfy at the same time.We filter out all false results meaning that there exists numbers that are non-pandigital and removed them from our list by replacing them with 1.
checkIfPandigital :: [Integer] -> [Integer]
checkIfPandigital n = if filter(\x -> x==False)[d234 n, d345 n, d456 n, d567 n, d678 n, d789 n, d890 n] == [] then n else [1]

euler43 = do
    -- Here we specified the digits used to create a pandigital number. It has to include 1 from the list [0 to 9].
    let pandigitalNo= [0..9]
    -- We created all possible canditates of pandigital numbers by using the permutation function.
    let permsPandigitalNo = permut pandigitalNo
    -- The permutation function includes numbers that start with 0 also. But as a condition all numbers must contain 10 digits and they can not start with 0. That's why we filtered out the ones that start with 0.
    let permsPandigitalNoNoZero = filter (\x -> (x !! 0) /= 0) permsPandigitalNo

    -- Once we obtain all permutations that are actually pandigital numbers by simply processing our possible
    --candidates with the given function definitions above, we obtained a list of numbers that are pandigitals
    -- and sum all of them up.
    let allPans = sum $ filter(\x -> x /= 1) (map(joiner) ((map(checkIfPandigital) permsPandigitalNoNoZero)))
    -- The result we have obtained is : 4453401842250219
    -- The upside of our algorithm is that it works accurately.
    -- The downside of our algorithm is that it works slow because it tries to compute all permutations of a 10 digit number.
    print allPans
    

    

--take digits from position 2,3,4 and check if resultant list is not prime:
d234 lst = not $ isPrime' (joiner([]++[lst!!1,lst!!2,lst!!3]))
--take digits from position 3,4,5:
d345 lst = not $ isPrime' (joiner([]++[lst!!2,lst!!3,lst!!4]))
--take digits from position 4,5,6:
d456 lst = not $ isPrime' (joiner([]++[lst!!3,lst!!4,lst!!5]))
--take digits from position 5,6,7:
d567 lst = not $ isPrime' (joiner([]++[lst!!4,lst!!5,lst!!6]))
--take digits from position 6,7,8:
d678 lst = not $ isPrime' (joiner([]++[lst!!5,lst!!6,lst!!7]))
--take digits from position 7,8,9:
d789 lst = not $ isPrime' (joiner([]++[lst!!6,lst!!7,lst!!8]))
--take digits from position 8,9,10:
d890 lst = not $ isPrime' (joiner([]++[lst!!7,lst!!8,lst!!9]))

main :: IO ()
main = do
    print ()