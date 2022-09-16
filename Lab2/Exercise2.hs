-- Time spent: 2 hours

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | not (((a + b > c) && (a + c > b)) && (b + c > a)) || ((a < 0 || b < 0 ) || c < 0) = NoTriangle
    | ((a==b) && (b==c)) && ((a>0 && b>0 ) && c>0) && ((a+b>c) || (a+c>b)) && (b+c>a) = Equilateral
    | (((((a==b) && (a/=c)) || ((a==c) && (a/=b))) || ((b==c) && (b/=a))) && ((a>0 && b>0 ) && c>0)) && (((a+b>c) || (a+c>b)) || (b+c>a) ) = Isosceles
    | (((a*a == (b*b + c*c) || b*b == (a*a + c*c)) || c*c == (a*a + b*b)) && ((a>0 && b>0 ) && c>0)) && (((a+b>c) || (a+c>b)) || (b+c>a) )  = Rectangular --right angled triangle
    | otherwise = Other 


genNoTriangle :: Integer -> [(Integer, Integer, Integer)]
genNoTriangle n = [(x, x, x * 2 + 1) | x <- [1..n]]

genEquilateral :: Integer ->  [(Integer, Integer, Integer)]
genEquilateral n = [(x, x, x) | x <- [1..n]]

genIsosceles :: Integer -> [(Integer, Integer, Integer)]
genIsosceles n = [(x*3, x*3, x*5) | x <- [1..n]]

genRectangular :: Integer -> [(Integer, Integer, Integer)]
genRectangular n = [(x*3, x*4, x*5) | x <- [1..n]]

testNoTriangle :: Integer -> [(Integer, Integer, Integer)]
testNoTriangle n =  filter (\(x,y,z) -> triangle x y z /= NoTriangle) (genNoTriangle n)

testEquilateral :: Integer -> [(Integer, Integer, Integer)]
testEquilateral n = filter (\(x,y,z) -> triangle x y z /= Equilateral) (genEquilateral n)

testIsosceles :: Integer -> [(Integer, Integer, Integer)]
testIsosceles n = filter (\(x,y,z) -> triangle x y z /= Isosceles) (genIsosceles n)

testRectangular :: Integer -> [(Integer, Integer, Integer)]
testRectangular n = filter (\(x,y,z) -> triangle x y z /= Rectangular) (genRectangular n)

-- We did not want to test our function by using the function, because if the triangle function is incorrect, then our tests will be obsolete. 
-- In other words, instead of reverse engineering the triangle function, we test it with different known domains.
-- The generators above, generates sequences of proven special triangles. They are strong preconditions, and while it would be better with weaker ones, this still tests that our function works with these predetermined triangles. 
-- As can be seen, there is not a generator for the Other type, this would be unecessary. Since, if a triangle does not fit in as a NoTriangle, Equilateral, Isoscoles or Rectangular, it's Other considered in the same domain.