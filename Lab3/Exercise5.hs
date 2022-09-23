-- Time spent: 120 minutes
module Exercise5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Maybe

import SetOrd

import Lecture3

import Exercise4(genSatisfiableComplexForm)
  

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- Below we will define properties in order to test the validity of the given sub function.

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
-- Converts an array of tokens to an array of their respective string representations.
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

-- The following function is used to find duplicates inside a list. nub and unique carry the same functionaliy
-- but unique retains the last occurence of each element 
-- https://stackoverflow.com/questions/3098391/unique-elements-in-a-haskell-list
unique = reverse . nub . reverse

-- Our sub2 function finds all subproblems of a form by also removing duplicates without using sets
-- We use the function unique to find all duplicates and we also add all subproblems to a list to find the length
-- We then write a test further down which is used to test our sub problem function against the given sub problem function that uses sets
-- We do this by comparing the lengths of the final set and list of subproblems found

-- Another note is that since both sub functions only accept main connectors with two atoms, we had to tune our generator in only generating
-- forms with two atoms. 
sub2 :: Form -> [Form]
sub2 (Prop x) = [Prop x]
sub2 (Neg f) =  unique (( Neg f):(sub2 f))
sub2 f@(Cnj [f1,f2]) = unique ( unique ( f :(sub2 f1)) ++(sub2 f2))
sub2 f@(Dsj [f1,f2]) = unique ( unique ( f :(sub2 f1)) ++(sub2 f2))
sub2 f@(Impl f1 f2) = unique ( unique ( f :(sub2 f1)) ++(sub2 f2))
sub2 f@(Equiv f1 f2) = unique ( unique ( f :(sub2 f1)) ++(sub2 f2))

findlength (Set (x:xs)) = (length (x:xs))

nsub :: Form -> Int
nsub f = length (sub2 f)

testSubformulasLength :: Form -> Bool
testSubformulasLength f = nsub (f) == findlength (sub f)

main :: IO () 
main = do 
    -- all test passed successfully 
    quickCheck $ forAll genSatisfiableComplexForm propAllAtomsAreIncluded
    quickCheck $ forAll genSatisfiableComplexForm allConectivesArePresent
    quickCheck $ forAll genSatisfiableComplexForm testSubformulasLength
