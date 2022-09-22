import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Maybe

import SetOrd

import Lecture3
import Exercise1
import Exercise3
import Exercise4
  

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

--Property 1: Are all atoms included in the output
propAllAtomsAreIncluded :: Form -> Bool 
propAllAtomsAreIncluded f = all (\x -> inSet (Prop x) (sub f)) (propNames f)

--Property 2: All connectives of form should be present in the subforms
allConectivesArePresent :: Form -> Bool
allConectivesArePresent f = connectorsInSet (tokensToStrings $ stripForm f) (sub f)

-- Helper functions for property 2

-- Extract the tokens from a form
stripForm :: Form -> [Token] 
stripForm f = filter (\x -> x /= TokenOP && x /= TokenCP && not (isDigit (last $ show x))) (lexer (showLst [f]))

-- Tokens to strings function
tokensToStrings :: [Token] -> [String]
tokensToStrings [] = []
tokensToStrings (t:ts)
    | t == TokenCnj = "*" : tokensToStrings ts
    | t == TokenDsj = "+" : tokensToStrings ts
    | t == TokenNeg = "-" : tokensToStrings ts
    | t == TokenImpl = "==>" : tokensToStrings ts
    | t == TokenEquiv = "<=>" : tokensToStrings ts
    | otherwise = "": tokensToStrings ts

-- The following functions tries to find a substring from a string
-- If the substring is not present in the string it will return -1 
-- else it will return the index of it 
-- source: https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell
findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

-- If a connector (string) is present in the subset/set (string)
-- it will return True otherwise False
connectorInSet :: String -> Set Form -> Bool
connectorInSet s setForm 
    | findString s (show setForm) == -1 = False
    | otherwise = True

-- Similar with the previous function. The only difference is that 
-- it works form multiple connectors (or strings) and they all need 
-- to be present to the set 
connectorsInSet :: [String] -> Set Form -> Bool
connectorsInSet [] setForm = True
connectorsInSet (x:xs) setForm = connectorInSet x setForm && connectorsInSet xs setForm 

main :: IO () 
main = do 
    -- all test passed successfully 
    quickCheck $ forAll genSatisfiableComplexForm propAllAtomsAreIncluded
    quickCheck $ forAll genSatisfiableComplexForm allConectivesArePresent