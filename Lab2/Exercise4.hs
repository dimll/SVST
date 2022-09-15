-- Time spent: 2 hours

import Data.List
import Data.Char
import Test.QuickCheck

-- https://stackoverflow.com/questions/14688716/removing-the-first-instance-of-x-from-a-list
-- This function removes the first insance of a number in a list
-- This helps the isPermutation function by allowing duplicates to be allowed in the permuatated list
deleteFirst _ [] = [] 
deleteFirst a (b:bc) | a == b = bc 
                     | otherwise = b : deleteFirst a bc

-- This only works with a list that does not have duplicates
isPermutation :: Eq a  => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] y = False
isPermutation (x:xs) y =
    if (x `elem` y)
        then isPermutation xs (deleteFirst x y)
        else False

propIsSortedListsEq :: [Integer] -> [Integer] -> Bool
propIsSortedListsEq a1 a2 = sort a1 == sort a2 

propIsListInAllPermutations :: [Integer] -> [Integer] -> Bool 
propIsListInAllPermutations a1 a2 = a1 `elem` permutations a2

propIsSameLength :: [Integer] -> [Integer] -> Bool 
propIsSameLength a1 a2 = length a1 == length a2  

propIsSameSum :: [Integer] -> [Integer] -> Bool 
propIsSameSum a1 a2 = sum a1 == sum a2  

testPropIsSortedListsEq :: Property
testPropIsSortedListsEq = forAll orderedList (\xs -> isPermutation xs (reverse xs) && propIsSortedListsEq xs (reverse xs))

testIsListInAllPermutations :: Property 
testIsListInAllPermutations = forAll ((arbitrary :: Gen [Integer]) `suchThat` (\xs -> length xs < 10)) (\xs -> isPermutation xs (reverse xs) && propIsListInAllPermutations xs (reverse xs))

testPropIsSameLength :: Property
testPropIsSameLength = forAll orderedList (\xs -> isPermutation xs (reverse xs) && propIsSameLength xs (reverse xs))

testPropIsSameSum :: Property
testPropIsSameSum = forAll orderedList (\xs -> isPermutation xs (reverse xs) && propIsSameSum xs (reverse xs))

{- 
We have identified three property functions. 

The first one (propIsSortedListsEq) will sort the two lists and check if they are equal. If they are it will return true. 

The second one (propIsListInAllPermutations) will generate all of the permutations of the second paramter and check if the first paramter is an element of that list of permutations.

The third one (propIsSameLength) will check if the length of the two arrays are equal. 

The fourth one (propIsSameSum) will check if the sum of the elements are equal. 

The first two functions are comparable in terms of strength, but the first one is way more efficient and can do tests for the entire domain on a regular CPU.
This is because the second one grows exponentially in size of the generated list of permutations. 
We do not think that the two latter properties are comparable in terms of strength. 

But we are certain that the first function is the better test as it is one of the strongest, and it checks the following: 
1. That the elements of both lists are the same. 
2. That the length of both lists are the same. 
3. If the two prior checks are True then the sums will also be the same.

The properties in descending order of strength are: 
1. propIsSortedListsEq
2. propIsListInAllPermutations
3. propIsSameLength
4. propIsSameSum
-}

main :: IO ()
main = do 
    print "Check if isPermutation works with a couple of cases"
    print "Case 1: same list in different order. Should be True."
    print "Case 2: Same elements but different order and amount. Should be False."
    print "Case 3: Some of the same elements but additional element. Should be False"
    print "Case 4: Completely different lists. Should be False,"
    print "These tests do not fully ensure that it works for all domains but they are sufficient enough to test a mixture of strong and weak postconditions."
    print $ isPermutation [1,2,3,4,5] [5,4,3,2,1]
    print $ isPermutation [123,321,123] [321,123,321] 
    print $ isPermutation [9,8,7,6] [9,8,7,6,5] 
    print $ isPermutation [1,2,3] [9,8,7] 
    quickCheck testPropIsSortedListsEq
    quickCheck testIsListInAllPermutations
    quickCheck testPropIsSameSum
    quickCheck testPropIsSameLength