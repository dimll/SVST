module Exercise4 where

import Data.List
import Exercise2 (MTFunction, MTMutator, MTProp, countSurvivors, countSurvivors')
import Exercise3 (powerset)
import MultiplicationTable
import Mutation
import Test.QuickCheck

getPropertyPowerSet :: [[MTProp]]
getPropertyPowerSet = powerset multiplicationTableProps

propLabels :: [[Int]]
propLabels = powerset [1 .. length multiplicationTableProps]

iterateOverPowerSet :: [[MTProp]] -> MTMutator -> Gen [Float]
iterateOverPowerSet [] m = listOf (arbitrary :: Gen Float) `suchThat` null
iterateOverPowerSet (x : xs) m = do
  store <- survivorPercentage 100 x m multiplicationTable
  next <- iterateOverPowerSet xs m
  return $ store : next

zipStrengthWithLabel :: MTMutator -> Gen [(Float, [Int])]
zipStrengthWithLabel m = do
  psStrength <- iterateOverPowerSet getPropertyPowerSet m
  return $ sortBy sortByStrength $ init $ zip psStrength propLabels

sortByStrength :: (Float, [Int]) -> (Float, [Int]) -> Ordering
sortByStrength a b
  | fst a < fst b = GT
  | otherwise = LT

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