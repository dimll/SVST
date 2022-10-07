module Exercise4 where

import Data.List
import Exercise2 (MTFunction, MTMutator, MTProp, countSurvivors, countSurvivors')
import Exercise3 (powerset)
import MultiplicationTable
import Mutation
import Test.QuickCheck

-- getPropertyPowerSet: we used this function to get the power set of all properties for the function multiplication table.
getPropertyPowerSet :: [[MTProp]]
getPropertyPowerSet = powerset multiplicationTableProps

--propLabels: We labeled the properties on multiplication table so that we can trace the properties inside the operations.
propLabels :: [[Int]]
propLabels = powerset [1 .. length multiplicationTableProps]

--iterateOverPowerSet: We iterated over the power set we have created for the properties of multiplication table. We used the same logic we have applied for the 
--counting survivors in exercise 2. For each mutator we counted the number of survivor properties and called the survivorPercentage function to calculate the percentages.
iterateOverPowerSet :: [[MTProp]] -> MTMutator -> Gen [Float]
iterateOverPowerSet [] m = listOf (arbitrary :: Gen Float) `suchThat` null
iterateOverPowerSet (x : xs) m = do
  store <- survivorPercentage 100 x m multiplicationTable
  next <- iterateOverPowerSet xs m
  return $ store : next

--This is a helper function to help us trace elements by their labels. This way we can differentiate which strength belongs to which label.
zipStrengthWithLabel :: MTMutator -> Gen [(Float, [Int])]
zipStrengthWithLabel m = do
  psStrength <- iterateOverPowerSet getPropertyPowerSet m
  return $ sortBy sortByStrength $ init $ zip psStrength propLabels

--This is a sorting function we have written to sort the strengths.
sortByStrength :: (Float, [Int]) -> (Float, [Int]) -> Ordering
sortByStrength a b
  | fst a < fst b = GT
  | otherwise = LT

-- survivorPercentage: for each survivor property we calculated its percentage with this function.
survivorPercentage :: Integer -> [MTProp] -> MTMutator -> MTFunction -> Gen Float
survivorPercentage number ps m f = do
  mutations <- listOf (mutate' m ps f 5) `suchThat` (\x -> toInteger (length x) == number)
  let getSurvivingRows = filter or mutations
  return (((fromIntegral $ length getSurvivingRows :: Float) / (fromIntegral number :: Float)) * 100)

main :: IO ()
main = do
  remEle <- generate $ zipStrengthWithLabel removeElements
  print "Check strength order for mutator removeElements"
  print remEle
