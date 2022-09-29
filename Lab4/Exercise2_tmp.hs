import Data.List
import LTS    
import Test.QuickCheck

import Ex1

-- Generates the most generic IOLTS
-- That means that the set of input or/and output labels might 
-- be empty. Also some loops (same action between the same state)
-- may exist or some state might be never reached.
ltsGen :: Gen IOLTS
ltsGen = do
    t <- genTransitions
    return (createIOLTS t)

-- Generates a list of transitions
-- Makes use of the helper functions below
genTransitions :: Gen [LabeledTransition]
genTransitions = do 
    size <- choose(2,4)
    res <- vectorOf size genTransition
    return res

-- Generates a single transition 
-- Used the below helper functions
genTransition :: Gen LabeledTransition
genTransition = do 
    l1 <- genStatesList
    s1 <- elements l1
    s2 <- elements l1
    actionType <- genBool 
    action <- if (actionType) 
                then genInputLabel
                else genOutputLabel
    return (s1,action,s2)

-- Generate list of States 
-- The total length of list 
-- will be between 2 and 4
genStatesList :: Gen [State]
genStatesList = do 
    size <- choose(2,4)
    res <- vectorOf size genState
    return res

-- Generate boolean value 
-- It is used to decide whether to 
-- generate an input or an output 
genBool :: Gen Bool 
genBool = do 
    b <- arbitrary
    return b

-- Generates a random State 
-- meaning an Integer 
-- In that case it will be from 0 to 3
genState :: Gen State 
genState = do
    r <- choose(0,3)
    return r

-- Generates input label from a list of input labels
genInputLabel :: Gen Label
genInputLabel = do
    let inLabels = ["?coin", "?button", "?kick"]
    inLabel <- elements inLabels
    return inLabel 

-- Generates output label from a list of output labels
genOutputLabel :: Gen Label
genOutputLabel = do
    let outLabels = ["!coffee", "!tea", "!juice"]
    outLabel <- elements outLabels
    return outLabel 

main :: IO ()
main = do 
    quickCheck $ forAll ltsGen validateLTS
