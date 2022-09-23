-- Time spent: 45 minutes
module Exercise3 where

import Lecture3

-- The procedure here is inspired by the way you would do this manually. 
-- First step, identify all False rows in a truth table. 
-- Create conjunctions with the appropriate values in each row.
-- Combine the forms as disjunctions. 
-- Negate the whole expression and use De Morgans law. 

-- As you can see, instead of applying De Morgans law at the end, it is done initially and throughout.

-- Get all rows from the truth table that are logically equivalent to F
getFalseValues :: Form -> [Valuation]
getFalseValues f = filter (\v -> not $ evl v f) (allVals f)

-- Insert a row from the previous function as input
-- Create a disjunction with the values, and assign the True valuations as negative props.
createSubForm :: Valuation -> Form
createSubForm v = Dsj (map (\(x,y) -> if y then Neg (Prop x) else Prop x) v)

-- Get all False rows from the truth table of f. 
-- create forms out of each row by using the createSubForm function. 
-- Now we will have an array of Forms. 
-- Create a conjunctive form out of this array
cnf :: Form -> Form
cnf f = Cnj $ map createSubForm (getFalseValues f)

checkIfTruthTableMatches :: Form -> Bool 
checkIfTruthTableMatches f = allVals f == allVals (cnf f)

main :: IO () 
main = do 
    print "Check if the CNF of form2 (Lecture3.hs) is logically equivalent to the original form."
    print "We are using this particular form, because it is satisfiable and not a tautology."
    print "If these criteria didn't apply to the form, the function would return an empty form."
    print $ checkIfTruthTableMatches form2