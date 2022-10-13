module Exercise4 where

import Data.List
import Exercise2
import Exercise3
import SetOrd
import System.Random
import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

--Exercise 4:

--PART A:
--Every element "x" in the domain must be in relation (x,y)
--An example to serial relations : successor function
-- Serial relations are : irreflexive, antisymmetric
-- Additionally, y must be in the domain as well to be valid
-- so we must check for that as well.
isSerial :: (Eq a, Ord a) => [a] -> Rel a -> Bool
isSerial domain rl = (domain == nub (map fst rl)) && all (\(_, x) -> x `elem` domain) rl

--PART B:
isReflexive :: (Ord a) => [a] -> Rel a -> Bool
isReflexive domain rel = all (\x -> (x, x) `elem` rel) domain

--PART C
relR :: Int -> [Int] -> [Int]
relR n = map (`mod` n)

--Creates the relation R:
tuplesRelR :: Int -> [Int] -> [(Int, Int)]
tuplesRelR n domainR = zip domainR (relR n domainR)

--Ex : isSerial [1,2,3,4,5] (tuplesRelnR 4 [1,2,3,4,5])
--In order to prove that RelnR is serial, we test for random domain=random list of integers and random n=random number and check if for all it is serial
genNumber :: Gen Int
genNumber = (arbitrary :: Gen Int) `suchThat` (> 0)

genNumberList :: Gen [Int]
genNumberList = listOf genNumber `suchThat` (not . null)

-- In order for the relation R to be serial, n cannot be divisible by any of the elements in the domain.
-- An easy way to check this is to make sure that n is greater than the largest x in the domain (maximum).
-- This means, that if n is divisible with any of the elements in the domain, R is not serial.
qcIsSerial :: Gen Bool
qcIsSerial = do
  nLs <- genNumberList
  n <- genNumber `suchThat` (\x -> x > maximum nLs)
  return $ isSerial (nub nLs) (tuplesRelR n (nub nLs))

propReflexiveIsSerial :: Ord a => [a] -> Rel a -> Bool
propReflexiveIsSerial domain rel = isReflexive domain rel --> isSerial domain rel

propSerialLength :: Ord a => [a] -> Rel a -> Bool
propSerialLength domainA rl = length domainA <= length rl

main :: IO ()
main = do
  quickCheck qcIsSerial
