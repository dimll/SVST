module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO
import Data.Function

--Property1 : (\x -> xmod2==0) for x is even
prop1 :: Int -> Bool
prop1 n = (n `mod` 2) == 0

--Property2 : (\x -> even && x>3)
prop2 :: Int -> Bool
prop2 n = ((n `mod` 2) == 0) && (n>3)

--Property3 : (\x -> even || x> 3)
prop3 :: Int -> Bool
prop3 n =((n `mod` 2) == 0) || (n>3)

--Property4 : ((\x -> even && x>3) || even x)
prop4 :: Int -> Bool
prop4 n = (((n `mod` 2) == 0) && (n>3) ) || ((n `mod` 2) == 0) 

--pre-condition : p (-10<= x <=10)
p :: [Int]
p = [-10..10]

--Get output space of prop1:
test1 = length (filter (\x-> x==True) (map (prop1) p))
--Get output space of prop2:
test2 = length (filter (\x-> x==True) (map (prop2) p))
--Get output space of prop3:
test3 = length (filter (\x-> x==True) (map (prop3) p))
--Get output space of prop4:
test4 = length (filter (\x-> x==True) (map (prop4) p))


stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

type Poly = [(Float,Int)]

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