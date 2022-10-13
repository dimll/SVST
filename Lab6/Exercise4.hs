module Exercise4 where

import Data.List
import Exercise2
import Exercise3
import SetOrd
import System.Random
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck qcIsSerial

--Exercise 4:

--Ex : isSerial [1,2,3,4,5] (tuplesRelnR 4 [1,2,3,4,5])
--In order to prove that RelnR is serial, we test for random domain=random list of integers and random n=random number and check if for all it is serial
genNumber :: Gen Int
genNumber = (arbitrary :: Gen Int) `suchThat` (> 0)

genNumberList :: Gen [Int]
genNumberList = listOf genNumber `suchThat` (not . null)

qcIsSerial :: Gen Bool
qcIsSerial = do
  n <- genNumber
  nLs <- genNumberList
  return $ isSerial nLs (tuplesRelnR n nLs)

--PART A:
--Every element "x" in the domain must be in relation (x,y)
--An example to serial relations : successor function
-- Serial relations are : irreflexive, antisymmetric
-- Additionally, y must be in the domain as well to be valid
-- so we must check for that as well.
isSerial :: (Eq a, Ord a) => [a] -> Rel a -> Bool
isSerial domain rl = (domain == nub (map fst rl)) && all (\(_, x) -> x `elem` domain) rl

--PART B:
createReflexive :: a -> (a, a)
createReflexive a = (a, a)

isReflexive :: (Eq a, Ord a) => [a] -> Rel a -> Bool
isReflexive domainA rl = list2set (filter (`inSet` list2set (map createReflexive domainA)) rl) == list2set (map createReflexive domainA)

--Ex irreflexive : isIrReflexive [1,2,3] [(1,2),(1,1),(2,2)] returns True
--QUICKCHECKPROP1 : Check if reflexive. All reflexive relations are serial
-- Information about serial relations obtained from : http://users.cecs.anu.edu.au/~jks/LogicNotes/relations.html
isIrReflexive :: (Eq a, Ord a) => [a] -> Rel a -> Bool
isIrReflexive domainA rl = not (isReflexive domainA rl)

isSymmetric :: (Eq a, Ord a) => Rel a -> Bool
isSymmetric rl = subSet (list2set (symClos rl)) (list2set rl)

--QUICKCHECKPROP2 : Check if antisymmetric (may not be complete)
--In order to be antisymmetric = irreflexive + not symmetric
--isAntisymmetric domainA rl = if ((isIrReflexive domainA rl) && (not(isSymmetric rl))) then True else False

--QUICKCHECKPROP2 : The length of tuples's in [(a,b)] must be at least the length of the domain
lengthProperty domainA rl = length domainA <= length rl

--PART C
funcR :: Int -> [Int] -> [Int]
funcR n = map (mod n)

relnR :: Int -> [Int] -> [Int]
relnR n domainR = funcR n domainR

--Creates the reln R:
tuplesRelnR :: Int -> [Int] -> [(Int, Int)]
tuplesRelnR n domainR = zip domainR (relnR n domainR)

--https://gist.github.com/thekarel/9964975
--applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
--applyNtimes f 1 = main
--applyNtimes f n =(applyNtimes (n-1) main)
