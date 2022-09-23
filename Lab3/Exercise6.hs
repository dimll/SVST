-- Time spent: 90 minutes
module Exercise6 where

import Lecture3
import Exercise3(cnf)
import Exercise4(genSatisfiableComplexForm)
import Test.QuickCheck

type Clause  = [Int]
type Clauses = [Clause]


-- Because we know the form of the input we can make certain assumptions. 
-- The "outer layer" should always be a Cnj, followed by Dsj forms, containing Props and Neg Props.
-- If Form is Cnj, create an array and concat all child members. 
-- Child members are Dsj, map these into respective arrays and return the string interpretation of the Props/Neg Props and convert to int.
-- Neg and Prop cases, are there for handling inputs that are not CNF. These are not really needed.
cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg f) = [[negate $ read $ show f]]
cnf2cls (Cnj fs) = concatMap cnf2cls fs
cnf2cls (Dsj fs) = [map (read . show) fs]

-- Any formula to clause form 
anyFormToClauses :: Form -> Clauses 
anyFormToClauses f = cnf2cls (cnf f)


-- Following code is for testing: 
-- The strongest test we found, is to convert the clauses back to a form, and compare the result to see if they are equal.

-- Helper function for converting a Clause to an list of Form.
clauseToFormList :: Clause -> [Form]
clauseToFormList [] = []
clauseToFormList (x:xs) 
    | x > 0 =  Prop x : clauseToFormList xs
    | otherwise = Neg (Prop $ abs x) : clauseToFormList xs

-- Helper function for applying Dsj to the list of Forms from the previous function (clauseToFormList).
clauseToForm :: Clause -> Form
clauseToForm c = Dsj $ clauseToFormList c

-- Helper function for generating the final form by applying Cnj to the mapped result of clauseToForm
clausesToForm :: Clauses -> Form 
clausesToForm c = Cnj $ map clauseToForm c

-- Test function
-- Compares the truth table of the input form to the truth table of the same input form, but after applying the cnf2cls and clausesToForm function.
-- Compares the original to a "double parsed" version.
testReverseConversion :: Form -> Bool 
testReverseConversion f = allVals f == allVals (clausesToForm (anyFormToClauses f))


main :: IO () 
main = do 
    print "Testing the function with the generator from Exercise4.hs"
    quickCheck $ forAll genSatisfiableComplexForm testReverseConversion
