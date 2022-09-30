-- Time spent: 80 minutes
module Exercise2 where

import Exercise1 (validateLTS)
import LTS
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


-- Generic implementation if IOLTS below

-- Generates the most generic IOLTS
-- That means that the set of input or output labels might 
-- be empty. Also some loops (same action between the same state)
-- may exist or some state might be never reached.

ltsGenGeneric :: Gen IOLTS
ltsGenGeneric = do
    t <- genericLabeledTransitionGen 3
    return (createIOLTS t)

genericLabeledTransitionGen :: Int -> Gen [LabeledTransition]
genericLabeledTransitionGen n = do
  domain <- domainGen n
  labels <- labelsGen n
  s <- shuffle domain
  return (zip3 domain labels s)


main :: IO ()
main = do
  -- Both passed all tests with our validation function we 
  -- created in Exercise 1
  quickCheck $ forAll ltsGen validateLTS
  quickCheck $ forAll ltsGenGeneric validateLTS
