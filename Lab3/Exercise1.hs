-- Time spent: 30 minutes
module Exercise1
where

import Lecture3

-- Check that all outputs are false
contradiction :: Form -> Bool 
contradiction f = not $ all (\v -> evl v f) (allVals f)

-- Check that all outputs are true
tautology :: Form -> Bool 
tautology f = all (\v -> evl v f) (allVals f)

-- Use the disjunctive form of implication to see if f entails g.
entails :: Form -> Form -> Bool 
entails f g = not (all (\v -> evl v f) (allVals f)) || all (\v -> evl v g) (allVals g)

-- 
equiv :: Form -> Form -> Bool 
equiv f g = all (\v -> evl v f == evl v g) (allVals f)