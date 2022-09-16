module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO
import Data.Function

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
lookupTable (x:xs) (y:ys) = (x,y) : lookupTable xs ys

inpt :: [Char]
inpt = uppercaseAtoM ++ uppercaseNtoZ ++ lowercaseAtoM ++ lowercaseNtoZ ++ nums ++ symbols

out :: [Char]
out = uppercaseNtoZ ++ uppercaseAtoM ++ lowercaseNtoZ ++ lowercaseAtoM ++ nums ++ symbols

formTable = lookupTable inpt out ++ [(' ', ' ')]
--Encoding:
newLetter n =snd (( filter (\x -> fst(x) == n) ((lookupTable inpt out ) ++ [(' ', ' ')]) ) !! 0)

rot13 :: [Char] -> [Char]
rot13 = map newLetter

--Decoding:
oldLetter n =fst (( filter (\x -> snd(x) == n) ((lookupTable inpt out ) ++ [(' ', ' ')]) ) !! 0)

revrot13 :: [Char] -> [Char]
revrot13 = map oldLetter

--Prop 1: 2 successive applications of rot13 must return the same text (rot13 is also its own inverse)
testprop1 :: [Char] -> Bool
testprop1 n = rot13 (rot13 n) == revrot13 (rot13 n)

test :: Gen Char
test = elements ['a'..'z']

genStrings :: Gen String
genStrings = listOf test

-- We use quick check to verify our findings with the following test.
main :: IO () 
main = do 
    quickCheck $ forAll genStrings testprop1

-- Time spent: 1h