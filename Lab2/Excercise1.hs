import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

--Monads do not store the values or do not evaluate the values, they just display the outcome
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

--Step 1: We take each element of x and put them into intervals
func1 :: Float -> [Char]
func1 n = do
            if  n>0 && n<0.25
                then  "Interval 1"
            else if n>=0.25 && n<0.5
                then "Interval 2"
            else if n>0.5 && n<0.75
                then "Interval 3"
            else "Interval 4"

--Step 2: we take func2 which maps  func1 on every element in list x
func2 x = fmap func1 x

--Step 3: We count number of times each element occurs in a list
numTimesFound x xs = (length. filter (== x)) xs

-- When we test for 10000 cases, each quartile is roughly 2500
{- With larger intereations, we can verify that the probs function properly works and
    generates an almost equal amount of decimals in each quartile. 
-}

main :: IO ()
main = do
    var <- probs 100 -- we unwrapped IO float to [Float] using <-
    print(numTimesFound "Interval 1" (func2 $ var))
    print(numTimesFound "Interval 2" (func2 $ var))
    print(numTimesFound "Interval 3" (func2 $ var))
    print(numTimesFound "Interval 4" (func2 $ var))

-- Time spent : 1 hour