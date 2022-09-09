import Test.QuickCheck

import Data.List

----------------------- Exercise 7 - START ----------------------------

luhn :: Integer -> Bool
luhn n = compareChecksums $ convertIntToIntArray n

-- Source: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
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

-- We are testing our functions by performing the tests presented below

-- Valid Lugn Numbers
-- Source: https://www.dcode.fr/luhn-algorithm
validLuhnNumbers :: [Integer]
validLuhnNumbers = [1412, 15550684, 3845098536, 095534719570, 890305230745842, 1172930479145920, 333875529230383809] 

-- A valid American Express Card, Master Card, or Visa Card number must first satisfy the 
-- luhn algorithm. Valid test cases for each type of cards are presented below
-- Source: https://www.freeformatter.com/credit-card-number-generator-validator.html
americanExpValid, masterCardValid,visaValid :: [Integer]
americanExpValid = [378282246310005,371449635398431,378734493671000]
masterCardValid = [5307149794595276, 5302127968008420]
visaValid = [4539053627938021, 4916291466783958]

-- Helper function for testing. Must return true if all the data is valid
-- Takes a list of numbers to check and if they pass all tests it returns true, otherwise false
-- testingList: The list to be tested by algorithms 
-- Example: testingValidity isMaster masterCardValid
testingValidity :: (Integer -> Bool) -> [Integer] -> Bool
testingValidity validityFuncton testingList = length (filter (==True) (map validityFuncton testingList)) == length testingList

-- Time spent: 3h
----------------------- Exercise 7 - END ----------------------------
