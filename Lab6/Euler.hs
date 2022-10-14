module Euler where
import System.Random
import Data.List

--Problem 619 : Square subsets
type Set a = [a]

powerset :: Set a -> Set (Set a)
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

lst :: [Int]
lst = [1000..234]
lst2 = [5..10]

productOfSubsets :: [Int]
productOfSubsets = map (foldr (*) 1) (powerset lst)

indexedProducts :: [(Int,Int)]
indexedProducts= zip [0..(length(productOfSubsets)-1)] (productOfSubsets)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral


perfectSquares = filter(\(a,b)-> (isqrt b)*(isqrt b) == b) indexedProducts

main :: IO()
main = do
    print(((length perfectSquares)-1) `mod` 1000000007) -- -1 to remove []