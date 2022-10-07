module Exercise5 where

import Data.List
import Exercise2 (MTMutator)
import Exercise3
import MultiplicationTable
import Mutation (changeValueOfAnyElement, changeValueOfFirstElement, mutators)
import Test.QuickCheck

--Similar to the other exercises, we have created the power set for the mutations. The mutations are represented as their coverages over properties.
genPowerSet :: [MTMutator] -> Gen [[[Integer]]]
genPowerSet ms = do
  powerset <$> genIntsArray ms

--The conjecture for fitspec checks whether a subset of properties implies or equivalent to other properties. We get all possible combination of properties
--by using power set function. Later we compared and labeled them to check if they are equivalent or implication or if there is no relation= nothing. Finally in order to
-- print the equivalences and implications, we filtered out the nothings.
comparePowerSubSets :: [[[Integer]]] -> Gen [([[Integer]], String, [[Integer]])]
comparePowerSubSets [[]] = listOf (arbitrary :: Gen ([[Integer]], String, [[Integer]])) `suchThat` null
comparePowerSubSets [_ : _] = listOf (arbitrary :: Gen ([[Integer]], String, [[Integer]])) `suchThat` null
comparePowerSubSets [] = listOf (arbitrary :: Gen ([[Integer]], String, [[Integer]])) `suchThat` null
comparePowerSubSets (x : y : xs) = do
  let conj = createConjector x y
  next <- comparePowerSubSets (y : xs)
  return $ filter (\(_, x, _) -> x /= "nothing") $ conj : next

--createConjector :: this function labels properties as Equivalences, Implications or Nothing.
createConjector :: [[Integer]] -> [[Integer]] -> ([[Integer]], String, [[Integer]])
createConjector x y
  | x == y = (x, "Eq", y)
  | y `subset` x = (x, "Impl", y)
  | otherwise = (x, "nothing", y)

--This is a helper function to describe the subset relation of 2 lists.
subset :: [[Integer]] -> [[Integer]] -> Bool
subset x y = null (y \\ (x `intersect` y))

main :: IO ()
main = do
  --We have written 2 mutation test cases to show in a more clear way that our function achieves to provide the expected results.
  -- Shows implications and equivalence
  mutLst1 <- generate $ genPowerSet [changeValueOfAnyElement, changeValueOfFirstElement]
  print mutLst1
  -- Not the case when using the list of all mutators
  mutLstAll <- generate $ genPowerSet mutators
  print mutLstAll
