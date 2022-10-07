module Exercise3 where

import Data.List
import Exercise2 (MTFunction, MTMutator, MTProp)
import MultiplicationTable
import Mutation
import Test.QuickCheck

--mutesOnProp : takes an integer value for the multiplication table, a single property, list of mutations and a function to mutate on which in this case
--it is the multiplication table. It creates mutations on the function under test for a single property and does it for all mutations recursively.
mutesOnProp :: Integer -> MTProp -> [MTMutator] -> MTFunction -> Gen [Bool]
mutesOnProp number p [] f = (arbitrary :: Gen [Bool]) `suchThat` null
mutesOnProp number p (m : ms) f = do
  mutations <- mutate' m [p] f number
  next <- mutesOnProp number p ms f
  return $ mutations ++ next

--testManynProps : takes an a list of properties and list of mutations. It creates mutations on the function under test for a multiple property and does it for 
--all mutations recursively.
testManyProps :: [MTMutator] -> [MTProp] -> Gen [[Bool]]
testManyProps ms [] = listOf (arbitrary :: Gen [Bool]) `suchThat` null
testManyProps ms (p : ps) = do
  mss <- mutesOnProp 1000 p ms multiplicationTable
  next <- testManyProps ms ps
  return $ mss : next


--This is a helper function we have written to help us with type conversions.
convertBoolsArrayToIntsArray :: [[Bool]] -> Gen [[Integer]]
convertBoolsArrayToIntsArray [] = listOf (arbitrary :: Gen [Integer]) `suchThat` null
convertBoolsArrayToIntsArray (bs : bss) = do
  let store = boolsToInts bs 1
  next <- convertBoolsArrayToIntsArray bss
  return $ store : next

--This is a helper function we have written to help us with type conversions.
boolsToInts :: [Bool] -> Integer -> [Integer]
boolsToInts [] start = []
boolsToInts (b : bs) start = [start | not b] ++ boolsToInts bs (start + 1)

--genIntsArray: is applying the mutations on the properties and checks if there exists any property that has surviving mutants. If filters out the properties that 
-- actually failed for a given mutant.For each mutant it provides a list indicating covered properties.Each index of the returned list is a reference to a mutant.
--Index 1 for mutant 1, index 2 for mutant 2 and so on.
genIntsArray :: [MTMutator] -> Gen [[Integer]]
genIntsArray ms = do
  tests <- testManyProps ms multiplicationTableProps
  convertBoolsArrayToIntsArray tests

--powerset : returns the power set of the inputed list.
-- source: https://stackoverflow.com/questions/32575630/powerset-of-a-set-with-list-comprehension-in-haskell
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = [x : ps | ps <- powerset xs] ++ powerset xs

--we sorted the powerset to see which subset of the powerset is providing the highest coverage. That refers to minimal property set.
sortPowerSets :: [[Integer]] -> [[Integer]] -> Ordering
sortPowerSets a b
  | length (unionSubList a) > length (unionSubList b) = GT
  | otherwise = LT

--This is a helper function used in the sorting function.
unionSubList :: [[Integer]] -> [Integer]
unionSubList = foldr union []

main :: IO ()
main = do
  ints <- generate $ genIntsArray mutators
  print $ last $ sortBy sortPowerSets (powerset ints)
