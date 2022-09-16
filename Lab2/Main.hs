module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO
import Data.Function

--Testing QuickCheck
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse ( reverse xs) == xs

--Property for -->
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all
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
numTimesFound x xs = (length. filter (== x)) xs



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
-----------------------------------EXERCISE 3 ENDS-----------------------------------------
-----------------------------------EXERCISE 4 STARTS-----------------------------------------



-----------------------------------EXERCISE 4 ENDS-----------------------------------------
-----------------------------------EXERCISE 5 STARTS-----------------------------------------



-----------------------------------EXERCISE 5 ENDS-----------------------------------------
-----------------------------------EXERCISE 6 STARTS-----------------------------------------
--Specification/Explanation : Basically we first created our own lookUpTable. You can also find this look 
-- up table on wikipedia : https://en.wikipedia.org/wiki/ROT13
-- the way we did it is, we first created 4 lists 1 for upper case letters from a to m,
-- 1 for lower case letters from a to m.
-- 1 for uppper case letters from n to z.
-- 1 for lower case letters from n to z.
-- The cipher text- plain text conversation treats special characters like +,-,*,/,? as they are so we have
-- mapped these characters they are. Later we combined these lists mixing their position of appearance on 
-- the concatonated list. We used this list to map from input sentence to output sentence character by character.
-- we used the same list in reverse manner (from second element to first element of the tuple) when we decrypt the sentence.
-- we used filter to find the matching character took the first element of the tuple to match with the encoding element(second)
-- we used the second element of tuple to match with the decoding element (first)
-- to check we created properties prop1 and used quickCheck.
-- The first property defined in wikipedia is if you apply rot13 2 times you get the original text.

--Look up table:
uppercaseAtoM= ['A','B'..'M']
uppercaseNtoZ=['N'..'Z']
lowercaseAtoM=['a'..'m']
lowercaseNtoZ=['n'..'z']
nums=['1'..'9']
symbols=['*','?',',','.',':','-','_','=','+','-','/']
-- Numbers and symbols are kept as they are. Only letter(upper and lower converted differently) are encoded.

lookupTable :: [a] -> [b] -> [(a,b)]
lookupTable [] _ = []
lookupTable (x:xs) (y:ys) = [(x,y)] ++ lookupTable xs ys

inpt :: [Char]
inpt = uppercaseAtoM ++ uppercaseNtoZ ++ lowercaseAtoM ++ lowercaseNtoZ ++ nums ++ symbols

out :: [Char]
out = uppercaseNtoZ ++ uppercaseAtoM ++ lowercaseNtoZ ++ lowercaseAtoM ++ nums ++ symbols

formTable = (lookupTable inpt out ) ++ [(' ', ' ')]
--Encoding:
newLetter n =snd (( filter (\x -> fst(x) == n) ((lookupTable inpt out ) ++ [(' ', ' ')]) ) !! 0)

rot13 :: [Char] -> [Char]
rot13 n = map(newLetter) n

--Decoding:
oldLetter n =fst (( filter (\x -> snd(x) == n) ((lookupTable inpt out ) ++ [(' ', ' ')]) ) !! 0)

revrot13 :: [Char] -> [Char]
revrot13 n = map(oldLetter) n

--Prop 1: 2 successive applications of rot13 must return the same text (rot13 is also its own inverse)
testprop1 :: [Char] -> Bool
testprop1 n = (rot13 ( rot13 n)) == (revrot13 (rot13 n))


-- to test the testprop1 : in prelude : quickCheck $ forAll genStrings $ testprop1  
test :: Gen Char
test = elements ['a'..'z']

genStrings :: Gen String
genStrings = listOf test


-----------------------------------EXERCISE 6 ENDS-----------------------------------------

-----------------------------------EXERCISE 7 STARTS -------------------------------------------


----------------------------------EXERCISE 7 ENDS -------------------------------------------

-----------------------------------EXERCISE 8 STARTS-----------------------------------------
--Euler 1: Multiples of 3 or 5
multiple3 :: Int -> [Int]
multiple3 n = (filter (<n) (map(*3) [1..n]))

multiple5 :: Int -> [Int]
multiple5 n = (filter (<n) (map(*5) [1..n]))

multiple3or5 n = (multiple3 n) `union` (multiple5 n)

res = sum (multiple3or5 1000)



--Euler 6: Sum Square Difference

square n =n*n
squares n= map (square) [1..n]
res1 n = sum (squares n)

sumN n = sum [1..n]
res2 n = square (sumN n)

diff n = res2 n - res1 n



-----------------------------------EXERCISE 8 ENDS-----------------------------------------

main :: IO ()
main = do
    ---- CALL FOR EXERCISE 1 ---------
    var <- probs 1000 -- we unwrapped IO float to [Float] using <-
    --print(var) --List created randomly each time by probs func

    print(numTimesFound "Interval 1" (func2 $ var))
    print(numTimesFound "Interval 2" (func2 $ var))
    print(numTimesFound "Interval 3" (func2 $ var))
    print(numTimesFound "Interval 4" (func2 $ var))

    --FOR EX3:
    let dict=[("Prop1",test1),("Prop2",test2),("Prop3",test3),("Prop4",test4) ]

    print(mySort dict)
    --To check :
    print(weaker p prop3 prop1)
    print(weaker p prop1 prop4)
    print(weaker p prop4 prop2)


    --- CALL FOR EXERCISE 2 ---------
