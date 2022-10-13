module Exercise6 where
import Test.QuickCheck
import System.Random
import Exercise3
import Exercise5
import SetOrd
import Data.List

-- 
genRandomTuple :: Gen ((Int, Int))
genRandomTuple = do
    a <- choose (1,10)
    b <- choose (1,10)
    return ((a,b))

genBinaryRelations :: Gen [(Int, Int)]
genBinaryRelations = do
    r <- listOf (genRandomTuple) `suchThat` (\x -> length x < 100 && length x >= 3 )
    return (nub (r))

propTestSymClosLength :: Ord a => (Rel a) -> Bool
propTestSymClosLength rl = length (symClos rl) >= (length (rl))

propTestElementsPresentSymClos :: Ord a => (Rel a) -> Bool
propTestElementsPresentSymClos rl = length (filter (\(x) -> x `elem` rl) (symClos rl)) == length (rl)

propCheckRSubsetToR2SymClos :: Ord a => (Rel a) -> Bool
propCheckRSubsetToR2SymClos rl = list2set (rl) `subSet` list2set (symClos (rl))



propTestTrClosLength :: Ord a => (Rel a) -> Bool
propTestTrClosLength rl = length (trClos rl) >= (length (rl))

propTestElementsPresentTrClos :: Ord a => (Rel a) -> Bool
propTestElementsPresentTrClos rl = length (filter (\(x) -> x `elem` rl) (trClos rl)) == length (rl)

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
