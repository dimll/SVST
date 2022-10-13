-- Time spent: 20mins

module Exercise3 (symClos) where

import Test.QuickCheck
import System.Random
import Exercise2
import SetOrd
import Data.List
import Data.Tuple


type Rel a = [(a,a)]

-- Using swap we are reversing the tuples, going from (a,b)
-- to the symmetric (b,a) and after that we are adding the given 
-- tuples. Next we are removing the duplicates that will occur 
-- in case that the initial pairs already contain one or more 
-- symmetric values
symClos :: Ord a => Rel a -> Rel a
symClos rel = nub $ map swap rel ++ rel

-- Example
main :: IO ()
main = do 
    let relDummy = [(1,2),(2,3),(3,4)]
    print (symClos relDummy)
