module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO

--Testing QuickCheck

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse ( reverse xs) == xs

-----------------------------------EXERCISE 1 STARTS-----------------------------------------
--Monads do not store the values or do not evaluate the values, they just display the outcome
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)




--Step 1: we take func2 which maps  func1 on every element in list x
func2 x = fmap func1 x

--Step 2: We take each element of x and put them into intervals
func1 :: Float -> [Char]
func1 n = do
            if  n>0 && n<0.25
                then  "Interval 1"
            else if n>=0.25 && n<0.5
                then "Interval 2"
            else if n>0.5 && n<0.75
                then "Interval 3"
            else "Interval 4"


--Step 3: We count number of times each element occurs in a list
numTimesFound x xs = (length . filter (== x)) xs



-----------------------------------EXERCISE 1 ENDS-----------------------------------------

-----------------------------------EXERCISE 2 STARTS-----------------------------------------
data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | ((((a+b>c) && (a+c>b)) && (b+c>a))==False) || ((a<0 || b<0 ) || c<0) = NoTriangle
    | (((a==b) && (b==c)) && ((a>0 && b>0 ) && c>0) && (((a+b>c) || (a+c>b))) && (b+c>a))= Equilateral
    | (((((a==b) && (a/=c)) || ((a==c) && (a/=b))) || ((b==c) && (b/=a))) && ((a>0 && b>0 ) && c>0)) && (((a+b>c) || (a+c>b)) || (b+c>a) )= Isosceles
    | (((a*a == (b*b + c*c) || b*b == (a*a + c*c)) || c*c == (a*a + b*b)) && ((a>0 && b>0 ) && c>0)) && (((a+b>c) || (a+c>b)) || (b+c>a) )  = Rectangular --right angled triangle
    | otherwise = Other


-- I've checked them manually they work.But how to automatically check them ?

-----------------------------------EXERCISE 2 ENDS-----------------------------------------
-----------------------------------EXERCISE 3 STARTS-----------------------------------------
--Q1: (\ x -> even x && x > 3) or even
--Q2: (\ x -> even x || x > 3) or even
--Q3: (\ x -> (even x && x > 3) || even x) or even
--Q4: even or (\ x -> (even x && x > 3) || even x)

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
--p:: a-> Bool
--p x =(x>= (-10)) && (x<10)

--post-condition : q 
--q:: a-> Bool

--p x =(x>= (-10)) && (x<10)

--stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
--stronger xs p q = all xs (\ x -> p x --> q x)
--weaker   xs p q = stronger xs q p 

-----------------------------------EXERCISE 3 ENDS-----------------------------------------
main :: IO ()
main = do
    ---- CALL FOR EXERCISE 1 ---------
    var <- probs 1000 -- we unwrapped IO float to [Float] using <-
    print(var) --List created randomly each time by probs func

    print(numTimesFound "Interval 1" (func2 $ var))
    print(numTimesFound "Interval 2" (func2 $ var))
    print(numTimesFound "Interval 3" (func2 $ var))
    print(numTimesFound "Interval 4" (func2 $ var))

    --- CALL FOR EXERCISE 2 ---------
