module Exercise6 where
import Test.QuickCheck
import System.Random
import Exercise3
import Exercise5
import SetOrd
import Data.List

-- A quick generator is used to generate one tuple of two random integers
-- Choose is used to specify the range of randomness between an integer of 1 to 10
genRandomTuple :: Gen ((Int, Int))
genRandomTuple = do
    a <- choose (1,10)
    b <- choose (1,10)
    return ((a,b))

-- Another quick check generator is used to generate binary relations
-- The genRandomTuple function is used to create one tuple of random integers, and then listOf is used to create multiple instances
-- The list of tuples is limited to be greater than a length of 3 and less than 100 to make debugging and analyzing tests easier
-- All duplicates are also removed from the list to simulate a set when testing properties with QuickCheck
-- Removing all duplicates also leads as to why we chose a length of 3
   -- Testing the transitive property requires a set of at least 2 elements but removing duplicates in some generated lists that have a length of 2
   -- will make them a length of 1 resulting in failed tests
genBinaryRelations :: Gen [(Int, Int)]
genBinaryRelations = do
    r <- listOf (genRandomTuple) `suchThat` (\x -> length x < 100 && length x >= 3 )
    return (nub (r))

-- This property tests that the length of the symmetric closure of a relation is greater than or equal to the length of the inital relation
propTestSymClosLength :: Ord a => (Rel a) -> Bool
propTestSymClosLength rl = length (symClos rl) >= (length (rl))

-- This property tests that all elements of the original relation exist within the symmetric closure of a relation
propTestElementsPresentSymClos :: Ord a => (Rel a) -> Bool
propTestElementsPresentSymClos rl = length (filter (\(x) -> x `elem` rl) (symClos rl)) == length (rl)

-- This property tests that the initial relation, R, is a subset of the symmetric closure of a relation, R2
propCheckRSubsetToR2SymClos :: Ord a => (Rel a) -> Bool
propCheckRSubsetToR2SymClos rl = list2set (rl) `subSet` list2set (symClos (rl))


-- This property tests that the length of the transitive closure of a relation is greater than or equal to the length of the initial relation
propTestTrClosLength :: Ord a => (Rel a) -> Bool
propTestTrClosLength rl = length (trClos rl) >= (length (rl))

-- This property tests that the all elements of the original relation exist within the transitive closure of a relation
propTestElementsPresentTrClos :: Ord a => (Rel a) -> Bool
propTestElementsPresentTrClos rl = length (filter (\(x) -> x `elem` rl) (trClos rl)) == length (rl)

-- This property tests that the relation of R2 is a subset of the transitive closure of the relation, R^n
-- The function make transitive takes the initial relation, R, and transforms into a transitive relation, R2
propCheckR2SubsetToRNTrClos :: Ord a => (Rel a) -> Bool
propCheckR2SubsetToRNTrClos rl = list2set (maketransitive (rl)) `subSet` list2set (trClos (rl))

main :: IO ()
main = do
    quickCheck $ forAll genBinaryRelations propTestSymClosLength
    quickCheck $ forAll genBinaryRelations propTestElementsPresentSymClos
    quickCheck $ forAll genBinaryRelations propCheckRSubsetToR2SymClos
    quickCheck $ forAll genBinaryRelations propTestTrClosLength
    quickCheck $ forAll genBinaryRelations propTestElementsPresentTrClos
    quickCheck $ forAll genBinaryRelations propCheckR2SubsetToRNTrClos
