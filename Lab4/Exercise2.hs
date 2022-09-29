-- Time spent: 60 minutes

import LTS (IOLTS, Label, LabeledTransition, State, createIOLTS)
import Test.QuickCheck

ltsGen :: Gen IOLTS
ltsGen = do
  labeledTransitions <- labeledTransitionGen 5
  return (createIOLTS labeledTransitions)

domainGen :: Int -> Gen [State]
domainGen n = listOf ((arbitrary :: Gen Integer) `suchThat` (> 0)) `suchThat` (\xs -> length xs == n)

stringListGen :: Int -> Gen [String]
stringListGen n = listOf (listOf (choose ('a', 'z')) `suchThat` (\x -> length x == 5)) `suchThat` (\xs -> length xs == n)

labelsGen :: Int -> Gen [Label]
labelsGen n = do
  notations <- listOf (elements ["!", "?"]) `suchThat` (\xs -> length xs == n)
  names <- stringListGen n
  let labelTuples = zip notations names
  return (map (uncurry (++)) labelTuples)

labeledTransitionGen :: Int -> Gen [LabeledTransition]
labeledTransitionGen n = do
  states <- domainGen n
  labels <- labelsGen n
  return (zip3 states labels (tail states ++ [head states]))