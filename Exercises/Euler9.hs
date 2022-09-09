import Test.QuickCheck

import Data.List
---------------------------- BONUS 9 START ------------------------------------
-- Euler problem 9
-- https://projecteuler.net/problem=9

-- The maximum possible number for b is 499 since all numbers
-- are natural and a<b<c. So lets try b from 2 to 499
-- c <- [1..998]
bPossibleList :: [Integer]
bPossibleList = [2..499]

-- Also since a+b+c=1000 => c = 1000-a-b should be a constant
-- Finally from the PT: a^2+b^2 must be equal to c^2

-- Using haskell list comprehension
euler9  :: Integer
euler9 = head [a*b*c | b<-bPossibleList, a<-[1..b], let c = 1000-a-b, a^2+b^2 == c^2]

-- (Better) intresting solution: https://www.xarg.org/puzzle/project-euler/problem-9/



---------------------------- BONUS 9 END ------------------------------------
