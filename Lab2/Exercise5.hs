import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
(-->) :: Bool -> Bool -> Bool 
p --> q = not p || q 

-- Helper function for quickCheck tests
genEvenDerangements = orderedList `suchThat` (\xs -> even (length xs) && length xs < 10)


-- Exercise 5

-- testIdx function test whether π(x) != x. If this inequality
-- is true, it will return true. Basically we use it to filter 
-- the list of permutations based on the criterion that for no x 
-- in the list the property π(x)=x must be true.   
testIdx :: Eq a => [a] -> [a] -> Bool
testIdx [] [] = True
testIdx (x:xs) (y:ys)  
    | x==y = False
    | length xs /= length ys = False
    | otherwise = testIdx xs ys

-- isDerangment function checks whether one list is a derangement of another one
-- and returns True or False accordingly. The logic is to filter out the x's 
-- that π(x)=x applies for them and we are doing that using the function testIdx.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b 
    | testIdx a b && (b `elem` permutations a) = True
    | otherwise = False

-- deran function generates all the possible derangement of a list. Given an 
-- argument n it finds the derangement of list [0..n-1]

deran:: Int -> [[Int]]
deran n = [ x | x <-permutations [0..n-1], testIdx x [0..n-1]]


-- Properties 
propIsSameLength :: [Integer] -> [Integer] -> Bool 
propIsSameLength a1 a2 = isDerangement a1 a2 --> (length a1 == length a2)

testPropIsSameLength :: Property
testPropIsSameLength = forAll genEvenDerangements (\xs ->  propIsSameLength xs (reverse xs))

propIsSameSum :: [Integer] -> [Integer] -> Bool 
propIsSameSum a1 a2 = isDerangement a1 a2 --> (sum a1 == sum a2)  

testPropIsSameSum :: Property
testPropIsSameSum = forAll genEvenDerangements (\xs ->  propIsSameSum xs (reverse xs))

propIsIndicesDifferent :: [Integer] -> [Integer] -> Bool
propIsIndicesDifferent [] [] = True
propIsIndicesDifferent (x:xs) (y:ys) = isDerangement xs ys --> ((x /= y) && propIsIndicesDifferent xs ys)

testPropIsIndicesDifferent :: Property
testPropIsIndicesDifferent = forAll genEvenDerangements (\xs -> propIsIndicesDifferent xs (reverse xs))

propIsPermutation :: [Integer] -> [Integer] -> Bool
propIsPermutation a1 a2 = isDerangement a1 a2 --> (sort a1 == sort a2)

testPropIsPermutation :: Property
testPropIsPermutation = forAll genEvenDerangements (\xs ->  propIsPermutation xs (reverse xs))

{- 
Properties ranked by strength: 
1. Is a permutation. 
2. The same indices does not have the same values.
3. The sum is equal
4. The length of each list is equal

These quickCheck tests are not perfect, as every list that is being compared will have an even length. 
This means that we are limiting our domain by half. 
 -}

main :: IO ()
main = do
    let l1 = permutations [1,2,3]
    print (deran 4)
    print "Check that two lists that we know are derangements return True"
    print (isDerangement [1,2,3] [3,1,2])
    print "Check that two lists that are different lengths return False"
    print (isDerangement [1,2,3] [3,1,2,0])
    print "Check that two lists that are different sums return False"
    print (isDerangement [1,2,3] [3,1,4])
    print "Check that two lists that are different are permutations but not derangements return False"
    print (isDerangement [1,2,3] [3,2,1])
    print "Check that two lists that are different sums return False"
    print (isDerangement [1,2,3] [3,1,4])
    print "Our properties are: they have to be the same length, have the same sum, be permutations, and not have same elements in the same indices."
    print "Check that both lists are permutations of each other"
    quickCheck testPropIsPermutation
    print "Check that the lists does not share any values on the same indices."
    quickCheck testPropIsIndicesDifferent
    print "Check that the sum of each list is equal"
    quickCheck testPropIsSameSum
    print "Check that the length of each list is equal"
    quickCheck testPropIsSameLength