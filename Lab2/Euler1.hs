module Main where

import Test.QuickCheck
import Data.List
import System.Random
import System.IO
import Data.Function

multiple3 :: Int -> [Int]
multiple3 n = (filter (<n) (map(*3) [1..n]))

multiple5 :: Int -> [Int]
multiple5 n = (filter (<n) (map(*5) [1..n]))

multiple3or5 n = (multiple3 n) `union` (multiple5 n)

res = sum (multiple3or5 1000)
