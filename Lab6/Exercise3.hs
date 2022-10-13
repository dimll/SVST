module Exercise3 where

import Test.QuickCheck
import System.Random
import Exercise2
import SetOrd
import Data.List
import Data.Tuple

--Ex3 :
type Rel a = [(a,a)]

revrse :: Ord a => (a,a)-> (a,a)
revrse (a,b) = (b,a)
--We added the (a,b) to every (b,a) and then to remove duplicates we took the union.
--To make it symmetric closure: symmetric closure of a binary relation R on a set X is the smallest symmetric relation 
--onX that contains R.
symClos' :: Ord a => Rel a -> Rel a
symClos' rl = nub(union (rl ++ (map(revrse) rl)) rl)

-- Dimitris implementation 

-- Using swap we are reversing the tuples, going from (a,b)
-- to the symmetric (b,a) and after that we are adding the given 
-- tuples. After that we are demoving the duplicates that will occur 
-- in case that the initial pairs already contain one or more 
-- symmetric values
symClos :: Ord a => Rel a -> Rel a
symClos rel = nub $ map swap rel ++ rel

-- Example
main :: IO ()
main = do 
    let relDummy = [(1,2),(2,3),(3,4)]
    print (symClos relDummy)
