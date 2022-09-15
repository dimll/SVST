import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Exercise 5

-- testIdx function test whether π(x) != x. If this inequality
-- is true, it will return true. Basically we use it to filter 
-- the list of permutations based on the criterion that for no x 
-- in the list the property π(x)=x must be true.   
testIdx :: Eq a => [a] -> [a] -> Bool
testIdx [] [] = True
testIdx (x:xs) (y:ys)  
    | x==y = False
    | length xs /= length ys = False
    | otherwise = testIdx xs ys

-- isDerangment function checks whether one list is a derangement of another one
-- and returns True or False accordingly. The logic is to filter out the x's 
-- that π(x)=x applies for them and we are doing that using the function testIdx.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b 
    | testIdx a b && (b `elem` permutations a) = True
    | otherwise = False

-- deran function generates all the possible derangement of a list. Given an 
-- argument n it finds the derangement of list [0..n-1]

deran:: Int -> [[Int]]
deran n = [ x | x <-permutations [0..n-1], testIdx x [0..n-1]]

-- Properties 
-- The previous permuation properties of sum and length must apply. 

-- The number of derangements of an n-element set must be n! 
-- Source https://oeis.org/wiki/Number_of_derangements
--prop_deranLen

-- Helper factorial function 
-- Helper function
factorialOfInt :: Int->Int
factorialOfInt 0 = 1
factorialOfInt n = n * factorialOfInt (n - 1)

main :: IO ()
main = do
    let l1 = permutations [1,2,3]
    print (deran 4)
    --print (isDerangement [1,2,3] [3,1,2])
    --print (der [1,2,3] [2,1,2]) 
