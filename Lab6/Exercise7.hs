module Exercise7 where
import Test.QuickCheck
import System.Random
import Exercise2
import Exercise3
import Exercise4
import Exercise5(trClos)
import SetOrd
import Data.List

genRandomTuple :: Gen ((Int, Int))
genRandomTuple = do
    a <- choose (1,10)
    b <- choose (1,10)
    return ((a,b))

genBinaryRelations :: Gen [(Int, Int)]
genBinaryRelations = do
    r <- listOf (genRandomTuple) `suchThat` (\x -> length x < 100 && length x >= 3 )
    return (nub (r))

--1. We wrote a symmetric closure of transitive closure for a random domain and a random reln.
symTr :: (Ord a)=>Rel a -> Rel a
symTr rln= symClos (trClos rln)

--2. We wrote a transitive closure of symmetric closure  for a random domain and a random reln.
trSym :: (Ord a)=>Rel a -> Rel a
trSym rln = trClos (symClos rln)

--3. We check set difference. They are different for any random relation.
checkIfDifferent ::  (Ord a)=> Rel a -> Bool
checkIfDifferent rel =not ((isDifferent' (list2set ( (trSym rel)))  (list2set (((symTr rel))))) &&  (isDifferent' (list2set ( (symTr rel)))  (list2set (((trSym rel))))))

main :: IO ()
main = do
    quickCheck $ forAll genBinaryRelations checkIfDifferent

isDifferent' :: Ord a => Set a -> Set a -> Bool
isDifferent' (Set a) b = length (filter (\x -> not (x `inSet` b)) a) > 0