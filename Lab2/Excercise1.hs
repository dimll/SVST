import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

seperator :: IO [Float] -> IO [[Float]]
seperator n = do
    x1 <- n >>= \y -> return [x | x <- y, x > 0, x < 0.25]
    x2 <- n >>= \y -> return [x | x <- y, x >= 0.25, x < 0.5]
    x3 <- n >>= \y -> return [x | x <- y, x >= 0.5, x < 0.75]
    x4 <- n >>= \y -> return [x | x <- y, x >= 0.75, x < 1.0]
    x5 <- n >>= \y -> return [x | x <- y, not ((x > 0 && x < 0.25) || (x>= 0.25 && x < 0.5) || (x>= 0.5 && x < 0.75) || (x>= 0.75 && x < 1))]
    return [x1, x2, x3, x4, x5]

counter :: IO [[Float]] -> IO [Int]
counter a = a >>= \y -> return $ map length y

countProbs :: IO Int 
countProbs = probs 10000 >>= \y -> return $ length y 

