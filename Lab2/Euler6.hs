module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO
import Data.Function

square n =n*n
squares n= map (square) [1..n]
res1 n = sum (squares n)

sumN n = sum [1..n]
res2 n = square (sumN n)

diff n = res2 n - res1 n