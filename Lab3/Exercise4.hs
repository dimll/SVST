-- Time spent 80 minutes

module Exercise4 where 

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture3
import Exercise1(tautology)
import Exercise3(cnf)


-- There were intially problems with using the valuation function with a Gen monad. To fix this, we recreated the valuation in the Gen context
-- genNames generates a randomly sized list ranging from 1 to 3 with random atoms (props) inside ranging from 1 to 3
-- This the first key to creating a Gen friendly valuation function since it returns a Gen [Int]
-- In order for the generation to work with Exercise5.hs we changed the behavior to always return 2 props.
genNames :: Gen [Int]
genNames = do
        x <- choose(2,2)
        return [1..x]

-- We use the genValuation function to then generate the entire valuation
-- First the atoms, or props, are unpacked
-- Then the boolean value of each atom is found in a Gen Bool context
-- Finally, all of the boolean values are zipped with the unpacked atoms list to return the final Valuation tuple list
genValuation :: Gen Valuation
genValuation = do
    names <- genNames
    bools <- listOf (arbitrary :: Gen Bool) `suchThat` (\xs -> length xs == length names)
    return (zip names bools)

-- A simple form for all valuations is then created
-- The conjunction or disjunction operator is chosen at random and used to create the simple proposition
-- During the map function, if the boolean value of a prop is fales, it is negated and if true, it is kept the same
-- This is similiar to how we created our CNF solution in the sense that we are using the truth table to generate a chain of logical propositions
-- The difference is that we are using random connectors instead of just the conjunction one for chaining the propositions
genSimpleForm :: Gen Form
genSimpleForm = do 
    valuations <- genValuation
    operator <- elements [Dsj, Cnj]
    return (operator (map (\(x,y) -> if y then Prop x else Neg (Prop x)) valuations))
    
-- After a simple form is created, we can start creating "complex" forms in the sense of using implications and equivalents
-- A random connector of either implication or equivalent is chosen and then a simple propositional form is generated and unpacked
-- We can then create a more complex form by using the new operator that was chosen
genComplexForm :: Gen Form 
genComplexForm = do 
    operator <- elements [Impl, Equiv]
    f1 <- genSimpleForm
    operator f1 <$> genSimpleForm

-- It is also important to check that the generated complex proposition is not a tautology so it can be tested with a proposition in CNF
-- We do this by using another generator that generates only satisfiable but not tautological propositions
genSatisfiableComplexForm :: Gen Form 
genSatisfiableComplexForm = genComplexForm `suchThat` (\x -> satisfiable x && not (tautology x))

-- Once we have generated randomly, viable propositions, we can test to see if they are in CNF by comparing their valuations to see if they are equivalent
-- This is the strongest post-condition we could find.
testCNFTruthTableEq :: Form -> Bool
testCNFTruthTableEq f = allVals f == allVals (cnf f)

-- Weaker property: check that all atoms in both the original form and CNF form are equal.
testCNFHasTheSamePropNames :: Form -> Bool 
testCNFHasTheSamePropNames f = propNames f == propNames (cnf f)

main :: IO () 
main = do 
    print "Tests that all generated forms from genSatisfiableComplexForm are logically equivalent after having CNF applied to them."
    quickCheck $ forAll genSatisfiableComplexForm testCNFTruthTableEq
    quickCheck $ forAll genSatisfiableComplexForm testCNFHasTheSamePropNames