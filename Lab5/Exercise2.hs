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


-- mutatateOne function takes as an argument 
-- one property and the function under test (fut)
-- and generates the output of a hardcoded mutator 
-- (in this case the mutator is changeValueOfAnyElement)
-- We also hardcode the input argument (10) of the function under test 
-- because it does not matter for the testing. 
mutateOne :: ([Integer] -> Integer -> Bool)-> (Integer -> [Integer]) -> Gen (Maybe Bool)
mutateOne prop fut = mutate changeValueOfAnyElement prop fut 10

-- mutateMultiple function takes a list of properties as an 
-- argument and the function under test (fut) and generates 
-- the output list of a hardcoded mutator (the same that is defined
-- in mutateOne function). The length of the output list will be 
-- equal to the number of properties we are testing. In our case 
-- we have defined 5 properties so the length of the output list
-- will be 5. 
mutateMultiple :: [([Integer] -> Integer -> Bool)] ->(Integer -> [Integer]) -> Gen [Maybe Bool]
mutateMultiple [] fut = return []
mutateMultiple (x:xs) fut = do
    l <- mutateOne x fut 
    n <- mutateMultiple xs fut
    return $ l : n

-- mutateTimes executes the above function mutateMultiple N times 
-- The total number of mutants will be N*(the number of properties)
-- So for 5 properties if we input N=800 to the function 
-- the output lenght will have 800*5= 4000 elements
mutateTimes :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen [Maybe Bool]
mutateTimes 0 props fut = return []
mutateTimes x props fut = do 
    l <- mutateMultiple props fut
    n <- mutateTimes (x-1) props fut 
    return $ l ++ n 

-- Filters the list of Maybe booleans and keeps only the Just True
-- ones which are those that survived. 
filterSurv :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Int
filterSurv num props fut = do
    vect <- mutateTimes num props fut 
    return $ length $ filter (==Just True) vect

-- countSurvivors is the final function as requested for Exercise 2 
countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer
countSurvivors num props fut = do
    x <- filterSurv (fromIntegral(num)) props fut
    return $ toInteger x

--example of use
-- generate $ countSurvivors 1000 multiplicationTableProps multiplicationTable 




main :: IO()
main = do
    -- Displays the number of survivors for a property and 4000 mutants (5*800) 
    survivors <- generate $ countSurvivors 800 multiplicationTableProps multiplicationTable
    print (survivors)

