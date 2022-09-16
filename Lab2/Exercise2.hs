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


-- This generates a non valid triangle arary which we later use for tests
-- To create this function, we found a sequence of set of numbers that do not follow the triangle theorem 
genNoTriangle :: Integer -> [(Integer, Integer, Integer)]
genNoTriangle n = [(x, x, x * 2 + 1) | x <- [1..n]]

-- This generates a valid list of integers that satisfy the equilateral triangle theorem
-- This theorem states that all side lengths are equal, therefore we generate three equal side lengths
genEquilateral :: Integer ->  [(Integer, Integer, Integer)]
genEquilateral n = [(x, x, x) | x <- [1..n]]

-- This generates a valid list of integers that satisfy the isosceles triangle theorem
{- This theorem states that two sides are equal and than one side is different. In our case, we are generating a limited amount of 
    isosceles triangles since we found a sequence by scaling the smallest instance of numbers that satisfy the isosceles triangle theorem.
    This instance was the numbers 3, 3, 5.
-}
genIsosceles :: Integer -> [(Integer, Integer, Integer)]
genIsosceles n = [(x*3, x*3, x*5) | x <- [1..n]]

-- This generates a valid list of integers that satisfy the rectangular triangle theorem
{- In our case, we are generating a limited amount of right angle triangles since we found a sequence by scaling the smallest instance of numbers
    that satisfy the right angle triangle theorem.
    This instance was the numbers 3, 4, 5.
-}
genRectangular :: Integer -> [(Integer, Integer, Integer)]
genRectangular n = [(x*3, x*4, x*5) | x <- [1..n]]

-- Test Functions

-- Is valid if length of array is 0
testNoTriangle :: Integer -> [(Integer, Integer, Integer)]
testNoTriangle n =  filter (\(x,y,z) -> triangle x y z /= NoTriangle) (genNoTriangle n)

-- Is valid if length of array is 0
testEquilateral :: Integer -> [(Integer, Integer, Integer)]
testEquilateral n = filter (\(x,y,z) -> triangle x y z /= Equilateral) (genEquilateral n)

-- Is valid if length of array is 0
testIsosceles :: Integer -> [(Integer, Integer, Integer)]
testIsosceles n = filter (\(x,y,z) -> triangle x y z /= Isosceles) (genIsosceles n)

-- Is valid if length of array is 0
testRectangular :: Integer -> [(Integer, Integer, Integer)]
testRectangular n = filter (\(x,y,z) -> triangle x y z /= Rectangular) (genRectangular n)

-- We did not want to test our function by using the function, because if the triangle function is incorrect, then our tests will be obsolete. 
-- In other words, instead of reverse engineering the triangle function, we test it with different known domains.
-- The generators above, generates sequences of proven special triangles. They are strong preconditions, and while it would be better with weaker ones, this still tests that our function works with these predetermined triangles. 
-- As can be seen, there is not a generator for the Other type, this would be unecessary. Since, if a triangle does not fit in as a NoTriangle, Equilateral, Isoscoles or Rectangular, it's Other considered in the same domain.

-- Time Spent : 2 hours