-- Time spent: 25 mins
module Exercise1 (genSet, genSetWithoutQ) where

import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

-- Generator using QuickCheck module
-- We first generate a random positive number l 
-- that will be the length of the list and after 
-- that we define that each number of the list must be 
-- positive (to be in accordance with natural numbers)
genSet ::  Gen  (Set Int)
genSet = do 
    l <- arbitrary `suchThat` (>0)
    n <- vectorOf l (arbitrary `suchThat` (>0))
    return $ list2set n

-- Random generation without QuickCheck
-- Similar with the previous implementation we are generating 
-- first the list length (with a maximum length of 100) and after 
-- that we are generating random numbers for the list 
genSetWithoutQ :: IO (Set Int) 
genSetWithoutQ = do
    l <- genRandomInt
    rNum <- sequence $ replicate l $ randomRIO(1,100::Int)
    return $ Set $ sort $ nub $ rNum

-- Helper function: generates a number between [1,100]
genRandomInt :: IO Int 
genRandomInt = randomRIO (1,100) :: IO Int

-- We have used the [sequence $ replicate l $ ..] command 
-- from https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell

-- Examples of the two test generators
main :: IO ()
main = do
  -- Example without QuickCheck
  print "Random Set Int generation without QuickCheck: "
  x <- genSetWithoutQ
  print (x)
  -- Example with QuickCheck
  print "Random Set Int generation with QuickCheck: "
  x <- generate genSet
  print (x)
