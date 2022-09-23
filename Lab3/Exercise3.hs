-- Time spent: 45 minutes
module Exercise3 where

import Lecture3

-- Get all rows from the truth table that are logically equivalent to F
getFalseValues :: Form -> [Valuation]
getFalseValues f = filter (\v -> not $ evl v f) (allVals f)

-- Insert a row from the previous function as input
-- Create a conjunction with the values, and assign the False valuations as negative props.
createSubForm :: Valuation -> Form
createSubForm v = Cnj (map (\(x,y) -> if y then Prop x else Neg (Prop x)) v)

-- Get all False rows from the truth table of f. 
-- create forms out of each row by using the createSubForm function. 
-- Now we will have an array of Forms. 
-- Create a Disjuntive form out of this array and negate it. 
cnf :: Form -> Form
cnf f = Neg $ Dsj $ map createSubForm (getFalseValues f)

checkIfTruthTableMatches :: Form -> Bool 
checkIfTruthTableMatches f = allVals f == allVals (cnf f)
