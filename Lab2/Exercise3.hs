module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO
import Data.Function
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

--Property1 : (\x -> xmod2==0) for x is even
prop1 :: Int -> Bool
prop1 = even

--Property2 : (\x -> even && x>3)
prop2 :: Int -> Bool
prop2 n = even n && n > 3

--Property3 : (\x -> even || x> 3)
prop3 :: Int -> Bool
prop3 n = even n || n > 3

--Property4 : ((\x -> even && x>3) || even x)
prop4 :: Int -> Bool
prop4 n = (even n && (n>3) ) || even n 

--pre-condition : p (-10<= x <=10)
p :: [Int]
p = [-10..10]

--Get output space of prop1:
test1 = length (filter (== True) (map prop1 p))
--Get output space of prop2:
test2 = length (filter (== True) (map prop2 p))
--Get output space of prop3:
test3 = length (filter (== True) (map prop3 p))
--Get output space of prop4:
test4 = length (filter (== True) (map prop4 p))


--We have implemented each property as prop1, prop2, prop3 and prop4. At the top of each property you can find detailed 
--explanation on what they refer to. The test functions take the input domain of the precondition which was given in the question in exercise 3,
--and they substitute each of the input to map the result to the output domain. If you think of the output domain as a line representing integers,
-- you can visually see the coverage of each of the tests on the output domain. That's how we determine the strength of the functions. Not by the size but
-- by the coverage on integer domain.

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 


--We later used the stronger and weaker functions to verify our findings.Then we sorted them based on their strengths using the sort function
-- we got from stackoverflow : https://stackoverflow.com/questions/30380697/sort-tuples-by-one-of-their-elements-in-haskell
type Poly = [(Float,Int)]

--
mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` snd)

main :: IO()
main = do
    let dict=[("Prop1",test1),("Prop2",test2),("Prop3",test3),("Prop4",test4) ]

    print(mySort dict)
    --To check :
    print(weaker p prop3 prop1)
    print(weaker p prop1 prop4)
    print(weaker p prop4 prop2)