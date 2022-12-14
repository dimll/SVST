-- Time spent: 30 minutes
module Exercise1 where

import Lecture3

{- 
In order to solve question 1, we have write the following properties:
Since satisfiability was already defined, we have moved on and 
checked the meaning of tautology, contradiction
and logical entailment and logical equivalence.

The definition on contradiction is that, when all possible values are inserted
in the function definition, they all must return false. allVals function returns
all possible values of a n variable (or property) function interchanging True and False 
values to create a 2^n  input list.
Ex: For n=2 all possible values are : (True,True)  (True,False)  (False,True)  (False,False)
Then to check that if the function actually works we have inserted all values in 
contradiction function. We have put not$all infront of it because for all of them we want the function
to return False in ordr for the whole thing to evaluate to True.
Some example formulas that can be used for test cases for contradiction test are : 
form5= Cnj(p , Neg (p))
form6 = Neg (Dsj (p , Neg (p))
form7 = Cnj(Conj(p, Neg(p)), q)

Similarly for tautology, we have used allVals function to supplement with necessary inputs, then instead of
using not$all we used all, because we want the function to return all True in for all input values.
Some example formulas that can be used for test cases for tautology :
form8 = Dsj(p, Neg(p))
form9 = Dsj(Imp(p,q) , Imp(q,p))
form 10= Dsj(Dsj(p, Neg(p)), q)

Logical entailment is tested via implementing an implication function in haskell. If the truth table returns
values that describes a implication for all values generated by allVals then we can conclude that it is an 
logical entailment. To solve this problem we have generated a property ((not p) or q). To do this we put not$all
infront of the first statement and logically combined it with the all vaues of second statement with an or statement
Some example formulas that can be used for test cases for logical entailment:
form 11= p form12 = q
form 13=Neq(q)  form14= Neg(p)

Logical equivalency is achieved when all values returned by when function is exactly equall to the ones they 
have returned by the second function when the same logical values are supplemented to the both functions. Their
truth tables are exactly equal with each other. That's why we used == in order to pairwise check all values returned
by both functions and compare.
Some example formulas that can be used for test cases for logical equivalency :
form 15: Imp(p,q) and form16: Dsj (Neg(p),q)
form 17: Imp(Neg(p), Neg(q)) and form18: Imp(q,p)
 -}

-- Check that all outputs are false
contradiction :: Form -> Bool 
contradiction f = not $ all (\v -> evl v f) (allVals f)

-- Check that all outputs are true
tautology :: Form -> Bool 
tautology f = all (\v -> evl v f) (allVals f)

-- Use the disjunctive form of implication to see if f entails g.
entails :: Form -> Form -> Bool 
entails f g = not (all (\v -> evl v f) (allVals f)) || all (\v -> evl v g) (allVals g)

-- Compare every row in the truth table for each function and make sure they are equal.
equiv :: Form -> Form -> Bool 
equiv f g = all (\v -> evl v f == evl v g) (allVals f)