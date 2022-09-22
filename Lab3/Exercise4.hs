import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture3
import Exercise1
import Exercise3

genNames :: Gen [Int]
genNames = do
        x <- choose(1,3)
        return [1..x]

genValuation :: Gen Valuation
genValuation = do
    names <- genNames
    bools <- listOf (arbitrary :: Gen Bool) `suchThat` (\xs -> length xs == length names)
    return (zip names bools)

genSimpleForm :: Gen Form
genSimpleForm = do 
    valuations <- genValuation
    operator <- elements [Dsj, Cnj]
    return (operator (map (\(x,y) -> if y then Prop x else Neg (Prop x)) valuations))
    
genComplexForm :: Gen Form 
genComplexForm = do 
    operator <- elements [Impl, Equiv]
    f1 <- genSimpleForm
    operator f1 <$> genSimpleForm

genSatisfiableComplexForm :: Gen Form 
genSatisfiableComplexForm = genComplexForm `suchThat` (\x -> satisfiable x && not (tautology x))

testCNFTruthTableEq :: Form -> Bool
testCNFTruthTableEq f = allVals f == allVals (cnf f)

main :: IO () 
main = do 
    verboseCheck $ forAll genSatisfiableComplexForm testCNFTruthTableEq