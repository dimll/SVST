-- Time spent: 80 minutes
module Exercise2 where

import Exercise1 (validateLTS)
import LTS
import Test.QuickCheck

--ltsGen first calls labeledTransitionGen to create LTSs and then feeds them to createIOLTS in order to obtain IOLTSs.
ltsGen :: Gen IOLTS
ltsGen = do
  labeledTransitions <- labeledTransitionGen 5
  return (createIOLTS labeledTransitions)

--domainGen receives an integer as its input parameter and returns a list of generated states. It does this by creating random/arbitrary integers
--such that all generated numbers are greater than 0 and the list contains n number of elements. For example if you input n as 5, it will create 5 numbers
--that are greater than 0 as a list.
domainGen :: Int -> Gen [State]
domainGen n = listOf ((arbitrary :: Gen Integer) `suchThat` (> 0)) `suchThat` (\xs -> length xs == n)

--stringListGen receives an integer as its input parameter and returns a list of generated labels. The labels does not need to be meaningful. Therefore we choose to
--create strings that are 5 characters long and consist of characters from a to z. The string list contains n elements. For example if you input n as 5, it will create
--labels like ['abzxs','ghjdf','aserf','asdfr','tyghf']
stringListGen :: Int -> Gen [String]
stringListGen n = listOf (listOf (choose ('a', 'z')) `suchThat` (\x -> length x == 5)) `suchThat` (\xs -> length xs == n)

--labelsGen: we have 2 types of labels. Input labels which start with ? and output labels which start with !. Using this logic, we used the strings we have created
--with stringListGen. We have divided them to categories as input output labels.
labelsGen :: Int -> Gen [Label]
labelsGen n = do
  notations <- listOf (elements ["!", "?"]) `suchThat` (\xs -> length xs == n)
  names <- stringListGen n
  let labelTuples = zip notations names
  return (map (uncurry (++)) labelTuples)

--Combining all those functions above we generated labeled transitions composed of states and labels.
labeledTransitionGen :: Int -> Gen [LabeledTransition]
labeledTransitionGen n = do
  states <- domainGen n
  labels <- labelsGen n
  return (zip3 states labels (tail states ++ [head states]))

-- Generic Implementation of IOLTS below :
-- Generates the most generic IOLTSs, that means the following conditions possibly exist in the generated IOLTSs:
--a. the set of input or output labels might be empty.
--b. some loops (same action between the same state) may exist.
--c. some states might be never reached (orphans).

--ltsGenGeneric first creates random LTSs by calling genericLabeledTransitionGen and feeds them to createIOLTS function to obtain IOLTSs.
ltsGenGeneric :: Gen IOLTS
ltsGenGeneric = do
  t <- genericLabeledTransitionGen 3
  return (createIOLTS t)

--genericLabeledTransitionGen : generates state transitions between a starting state we obtain from domainGen, which returns a list of states,
--an end state which we obtain similarly by shuffling the list of states and a label in between the start and end states we obtain by labelsGen, which returns a list
--of labels. After all 3 components are obtained, we put them into the (start,label,end) format to create a state transition.
genericLabeledTransitionGen :: Int -> Gen [LabeledTransition]
genericLabeledTransitionGen n = do
  domain <- domainGen n
  labels <- labelsGen n
  s <- shuffle domain
  return (zip3 domain labels s)

main :: IO ()
main = do
  -- Inside main function we automated the checking procedure with QuickCheck.
  -- We have created 2 generators for valid IOLTS generation. Each generator generators IOLTSs. After creating multiple IOLTSs with QuickCheck,
  -- We called validateLTS function to validate whether or not our generators produce valid IOLTSs.
  -- Both generators have passed all tests with our validation function we created in Exercise 1.
  quickCheck $ forAll ltsGen validateLTS
  quickCheck $ forAll ltsGenGeneric validateLTS
