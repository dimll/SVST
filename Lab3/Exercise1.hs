import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

import Lecture3


contradiction :: Form -> Bool
contradiction f = not (all (\v -> evl v f)(allVals f))

tautology :: Form -> Bool
tautology f = (all(\v -> evl v f)(allVals f))

-- logical entailment 
entails :: Form -> Form -> Bool
entails p q = not (all(\v -> evl v p)(allVals p)) || (all(\v -> evl v q)(allVals q))

-- logical equivalence
equiv:: Form->Form->Bool 
equiv f1 f2 = all(\v -> evl v f1 == evl v f2)(allVals f1)

-- We are construction always True and always False forms 
-- to check the above definitions

-- Always True 
formT = Dsj [p ,(Neg p)]

-- Always False
formF = Cnj [p ,(Neg p)]

main :: IO ()
main = do
    print (tautology formT) -- Should be True
    print (tautology formF) -- Should be False
    print (contradiction formT) -- Should be False
    print (contradiction formF) -- Should be True
    print (entails formT formT) -- Should be True
    print (entails formT formF) -- Should be False
    print (entails formF formT) -- Should be True
    print (entails formF formF) -- Should be True
    print (equiv formT formT) -- Should be True
    print (equiv formT formF) -- Should be False
    print (equiv formF formT) -- Should be False 
    print (equiv formF formF) -- Should be True

 -- All the above definitions behave as expected so testing is complete   
