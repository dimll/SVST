-- Time spent: 25 mins
module Exercise1 (genSet) 

where

import SetOrd
import Data.List
import System.Random
import Test.QuickCheck

-- Generator using QuickCheck module
genSet ::  Gen  (Set Int)
genSet = do 
    l <- arbitrary `suchThat` (>0)
    n <- vectorOf l (arbitrary `suchThat` (>0))
    return $ list2set n

-- Random generation without QuickCheck
genRandomInt :: IO Int 
genRandomInt = randomRIO (1,100) :: IO Int

-- Source: https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
genSetWithoutQ :: IO (Set Int) 
genSetWithoutQ = do
    l <- genRandomInt
    rNum <- sequence $ replicate l $ randomRIO(1,100::Int)
    return $ Set rNum 


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
