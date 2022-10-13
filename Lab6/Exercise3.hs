module Exercise3 where
import Test.QuickCheck
import System.Random
import Exercise2
import SetOrd
import Data.List

--Ex3 :
type Rel a = [(a,a)]
revrse :: Ord a => (a,a)-> (a,a)
revrse (a,b) = (b,a)
--We added the (a,b) to every (b,a) and then to remove duplicates we took the union.
--To make it symmetric closure: symmetric closure of a binary relation R on a set X is the smallest symmetric relation 
--onX that contains R.
symClos :: Ord a => Rel a -> Rel a
symClos rl = nub(union (rl ++ (map(revrse) rl)) rl)
