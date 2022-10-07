module Exercise4 where

import Exercise2 (countSurvivors)
import Exercise3 (MTProp, powerset)
import MultiplicationTable
import Mutation
import Test.QuickCheck

getPropertyPowerSet :: [[MTProp]]
getPropertyPowerSet = powerset multiplicationTableProps

propLabels :: [[Int]]
propLabels = powerset [1 .. length multiplicationTableProps]

iterateOverPowerSet :: [[MTProp]] -> Gen [Integer]
iterateOverPowerSet [] = listOf (arbitrary :: Gen Integer) `suchThat` null
iterateOverPowerSet (x : xs) = do
  store <- countSurvivors 1000 x multiplicationTable
  next <- iterateOverPowerSet xs
  return $ store : next

zipStrengthWithLabel :: Gen [(Integer, [Int])]
zipStrengthWithLabel = do
  psStrength <- iterateOverPowerSet getPropertyPowerSet
  return $ init $ zip psStrength propLabels

main :: IO ()
main = do
  output <- generate zipStrengthWithLabel
  print output