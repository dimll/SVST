-- Time spent: 90 minutes
import Lecture3
import Exercise3
import Exercise4(genSatisfiableComplexForm)
import Test.QuickCheck

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg f) = [[negate $ read $ show f]]
cnf2cls (Cnj fs) = concatMap cnf2cls fs
cnf2cls (Dsj fs) = [map (read . show) fs]

-- Any formula to clause form 
anyToClause :: Form -> Clauses 
anyToClause f = cnf2cls (cnf f)

-- Test by converting clause to form
clauseToFormTemp :: Clause -> [Form]
clauseToFormTemp [] = []
clauseToFormTemp (x:xs) 
    | x > 0 =  Prop x : clauseToFormTemp xs
    | otherwise = Neg (Prop $ abs x) : clauseToFormTemp xs

clauseToForm :: Clause -> Form
clauseToForm c = Dsj $ clauseToFormTemp c

clausesToForm :: Clauses -> Form 
clausesToForm c = Cnj $ map clauseToForm c

testReverseConversion :: Form -> Bool 
testReverseConversion f = allVals f == allVals (clausesToForm (anyToClause f))


main :: IO () 
main = do 
    quickCheck $ forAll genSatisfiableComplexForm testReverseConversion
