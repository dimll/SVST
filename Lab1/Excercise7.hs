import Data.List
import Test.QuickCheck

luhn :: Integer -> Bool
luhn n = compareChecksums $ convertIntToIntArray n

getDigits :: Integral x => x -> [x]
getDigits 0 = []
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]

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

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = (luhn n) && length (show n) == 15 && (take 2 (getDigits n) == [3, 4] || take 2 (getDigits n) == [3, 7])

isMaster :: Integer -> Bool
isMaster n = (luhn n) && length (show n) == 16 && ((read (take 2 (show n))) >= 51 && (read (take 2 (show n))) <= 55 ) || ((read (take 2 (show n))) >= 2221 && (read (take 2 (show n))) <= 2720 )

isVisa :: Integer -> Bool
isVisa n = (luhn n) && (length (show n) == 16 || length (show n) == 13) && (take 1 (getDigits n) == [4])
