module Exercise3 where

import Data.List
import Exercise2 (MTFunction, MTMutator, MTProp)
import MultiplicationTable
import Mutation
import Test.QuickCheck

mutesOnProp :: Integer -> MTProp -> [MTMutator] -> MTFunction -> Gen [Bool]
mutesOnProp number p [] f = (arbitrary :: Gen [Bool]) `suchThat` null
mutesOnProp number p (m : ms) f = do
  mutations <- mutate' m [p] f number
  next <- mutesOnProp number p ms f
  return $ mutations ++ next

testManyProps :: [MTMutator] -> [MTProp] -> Gen [[Bool]]
testManyProps ms [] = listOf (arbitrary :: Gen [Bool]) `suchThat` null
testManyProps ms (p : ps) = do
  mss <- mutesOnProp 1000 p ms multiplicationTable
  next <- testManyProps ms ps
  return $ mss : next

convertBoolsArrayToIntsArray :: [[Bool]] -> Gen [[Integer]]
convertBoolsArrayToIntsArray [] = listOf (arbitrary :: Gen [Integer]) `suchThat` null
convertBoolsArrayToIntsArray (bs : bss) = do
  let store = boolsToInts bs 1
  next <- convertBoolsArrayToIntsArray bss
  return $ store : next

boolsToInts :: [Bool] -> Integer -> [Integer]
boolsToInts [] start = []
boolsToInts (b : bs) start = [start | not b] ++ boolsToInts bs (start + 1)

genIntsArray :: [MTMutator] -> Gen [[Integer]]
genIntsArray ms = do
  tests <- testManyProps ms multiplicationTableProps
  convertBoolsArrayToIntsArray tests

-- source: https://stackoverflow.com/questions/32575630/powerset-of-a-set-with-list-comprehension-in-haskell
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = [x : ps | ps <- powerset xs] ++ powerset xs

sortPowerSets :: [[Integer]] -> [[Integer]] -> Ordering
sortPowerSets a b
  | length (unionSubList a) > length (unionSubList b) = GT
  | otherwise = LT

unionSubList :: [[Integer]] -> [Integer]
unionSubList = foldr union []

main :: IO ()
main = do
  ints <- generate $ genIntsArray mutators
  print $ last $ sortBy sortPowerSets (powerset ints)