import Exercise1

import Mutation
import Test.QuickCheck
import Data.List
import MultiplicationTable
import Data.Maybe
import Debug.Trace



--countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> Integer

-- List of properties 
listOfProps :: [[Integer] -> Integer -> Bool]
listOfProps = multiplicationTableProps

-- Number of mutants
noOfMutants :: Integer
noOfMutants = 10

-- Function under test -> multiplicationTable 
-- mutate function inputs: mutator prop fut input

mutateOne :: ([Integer] -> Integer -> Bool)-> (Integer -> [Integer]) -> Gen (Maybe Bool)
mutateOne prop fut = mutate changeValueOfAnyElement prop fut 10


mutateMultiple :: [([Integer] -> Integer -> Bool)] ->(Integer -> [Integer]) -> Gen [Maybe Bool]
mutateMultiple [] fut = return []
mutateMultiple (x:xs) fut = do
    l <- mutateOne x fut 
    n <- mutateMultiple xs fut
    return $ l : n

mutateTimes :: Int ->[([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen [Maybe Bool]
mutateTimes 0 props fut = return []
mutateTimes x props fut = do 
    l <- mutateMultiple props fut
    n <- mutateTimes (x-1) props fut 
    return $ l ++ n 

filterSurv :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Int
filterSurv num props fut = do
    vect <- mutateTimes num props fut 
    return $ length $ filter (==Just True) vect

countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer
countSurvivors num props fut = do
    x <- filterSurv (fromIntegral(num)) props fut
    return $ toInteger x

--example of use
-- generate $ countSurvivors 1000 listOfProps multiplicationTable 




main :: IO()
main = do
    print ("Ex 2")


    --sample' $ mutate changeValueOfAnyElement MultiplicationTable.prop_sumIsTriangleNumberTimesInput  MultiplicationTable.multiplicationTable 1