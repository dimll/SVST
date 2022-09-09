-- Time spent: 120 minutes

import Data.List
import Test.QuickCheck

luhn :: Integer -> Bool
luhn n = compareChecksums $ convertIntToIntArray n

getDigits :: Integral x => x -> [x]
getDigits 0 = []
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]

-- Compare the payload checksum to the given checksum.
compareChecksums :: [Int] -> Bool
compareChecksums a = getPayloadChecksum (take (length a - 1) a) == last a

-- Helper function to cut down on redundant code
convertIntToIntArray :: Integer -> [Int]
convertIntToIntArray n = map (read . (: "")) (show n)

-- Calculate the checksum of the sum of the payload after appropriate calculations.
getPayloadChecksum :: [Int] -> Int
getPayloadChecksum a = (10 - getPayloadSum a `mod` 10) `mod` 10

-- The sum of the entire payload (complete Int - last digit)
getPayloadSum :: [Int] -> Int
getPayloadSum a = sum (sumIntsOver10 $ makeWeightedProducts a)

-- Map over array and do the correct calculations for each digit.
sumIntsOver10 :: [Int] -> [Int]
sumIntsOver10 = map digitSummation

-- If n > 9 return split the Int into a [Int] and return return the sum.
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

-- Check if first two digits are 34 or 37, length is 15 and passes Luhn.
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n && length (show n) == 15 && (take 2 (getDigits n) == [3, 4] || take 2 (getDigits n) == [3, 7])

-- Check if first digit is between 51 and 55 or 2221 and 2720, length is 16 and passes Luhn.
isMaster :: Integer -> Bool
isMaster n = luhn n && length (show n) == 16 && ((read (take 2 (show n)) >= 51 && read (take 2 (show n)) <= 55) || (read (take 2 (show n)) >= 2221 && read (take 2 (show n)) <= 2720))

-- Check if first digit is 4, length is 16 and passes Luhn.
isVisa :: Integer -> Bool
isVisa n = luhn n && (length (show n) == 16 || length (show n) == 13) && take 1 (getDigits n) == [4]
