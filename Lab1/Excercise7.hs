import Data.List
import Test.QuickCheck

luhn :: Integer -> Bool
luhn n = compareChecksums $ convertIntToIntArray n

compareChecksums :: [Int] -> Bool
compareChecksums a = getPayloadChecksum (take (length a - 1) a) == last a

convertIntToIntArray :: Integer -> [Int]
convertIntToIntArray n = map (read . (: "")) (show n)

getPayloadChecksum :: [Int] -> Int
getPayloadChecksum a = (10 - (getPayloadSum a `mod` 10)) `mod` 10

getPayloadSum :: [Int] -> Int
getPayloadSum a = sum (sumIntsOver10 $ makeWeightedProducts a)

sumIntsOver10 :: [Int] -> [Int]
sumIntsOver10 = map digitSummation

digitSummation :: Int -> Int
digitSummation n
  | n <= 9 = n
  | otherwise = sum $ convertIntToIntArray (toInteger n)

-- Get the product of the weights mulitplied by the input array
makeWeightedProducts :: [Int] -> [Int]
makeWeightedProducts a = zipWith (*) a (makeWeights a)

-- Make an array of weights where every other digit is 2 starting from the right
makeWeights :: [Int] -> [Int]
makeWeights a = reverse $ take (length a) (cycle [2, 1])