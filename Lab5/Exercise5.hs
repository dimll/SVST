module Exercise5 where

import Data.List
import Exercise2 (MTMutator)
import Exercise3
import MultiplicationTable
import Mutation (changeValueOfAnyElement, changeValueOfFirstElement, mutators)
import Test.QuickCheck

genPowerSet :: [MTMutator] -> Gen [[[Integer]]]
genPowerSet ms = do
  powerset <$> genIntsArray ms

comparePowerSubSets :: [[[Integer]]] -> Gen [([[Integer]], String, [[Integer]])]
comparePowerSubSets [[]] = listOf (arbitrary :: Gen ([[Integer]], String, [[Integer]])) `suchThat` null
comparePowerSubSets [_ : _] = listOf (arbitrary :: Gen ([[Integer]], String, [[Integer]])) `suchThat` null
comparePowerSubSets [] = listOf (arbitrary :: Gen ([[Integer]], String, [[Integer]])) `suchThat` null
comparePowerSubSets (x : y : xs) = do
  let conj = createConjector x y
  next <- comparePowerSubSets (y : xs)
  return $ filter (\(_, x, _) -> x /= "nothing") $ conj : next

createConjector :: [[Integer]] -> [[Integer]] -> ([[Integer]], String, [[Integer]])
createConjector x y
  | x == y = (x, "Eq", y)
  | y `subset` x = (x, "Impl", y)
  | otherwise = (x, "nothing", y)

subset :: [[Integer]] -> [[Integer]] -> Bool
subset x y = null (y \\ (x `intersect` y))

main :: IO ()
main = do
  -- Shows implications and equivalence
  mutLst1 <- generate $ genPowerSet [changeValueOfAnyElement, changeValueOfFirstElement]
  print mutLst1
  -- Not the case when using the list of all mutators
  mutLstAll <- generate $ genPowerSet mutators
  print mutLstAll
