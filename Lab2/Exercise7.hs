import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

countrycodes = [("AL",28),("AD",24),("AT",20),("AZ",28),("BH",22),("BY",28),("BE",16),("BA",20),("BR",29),("BG",22),("CR",22),("HR",21),("CY",28),("CZ",24),("DK",18),("DO",28),("EG",29),("SV",28),("EE",20),("FO",18),("FI",18),("FR",27),("GE",22),("DE",22),("GI",23),("GR",27),("GL",18),("GT",28),("VA",22),("HU",28),("IS",26),("IQ",23),("IE",22),("IL",23),("IT",27),("JO",30),("KZ",20),("XK",20),("KW",30),("LV",21),("LB",28),("LY",25),("LI",21),("LT",20),("LU",20),("MT",31),("MR",27),("MU",30),("MD",24),("MC",27),("ME",22),("NL",18),("MK",19),("NO",15),("PK",24),("PS",29),("PL",28),("PT",25),("QA",29),("RO",24),("LC",32),("SM",27),("ST",25),("SA",24),("RS",22),("SC",31),("SK",24),("SI",19),("ES",24),("SD",18),("SE",24),("CH",21),("TL",23),("TN",24),("TR",26),("UA",29),("AE",23),("GB",22),("VG",24)]

iban :: String -> Bool
iban n = checkLengthByCountry n && computeRemainder n

-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe
-- Lookup length of country by their country code
getCountryLength :: String -> Maybe Int
getCountryLength n = lookup (take 2 n) countrycodes

-- Check if the length associated to the chosen country is valid for the entire string
checkLengthByCountry :: String -> Bool 
checkLengthByCountry n = case getCountryLength n of 
    Nothing -> False
    Just x -> length n == x 

-- Move 4 characters from the start to the end of the string.
move4CharactersToEnd :: String -> String 
move4CharactersToEnd n = drop 4 n ++ take 4 n

-- Convert letters to numeric value A=10, B=11, etc.
convertLettersToNumericValue :: String -> String 
convertLettersToNumericValue = concatMap (\x -> if ord x > 57 then show $ ord x - 55 else show $ ord x - 48) 

-- Return true if remainder is correct
computeRemainder :: String -> Bool 
computeRemainder n = read ( convertLettersToNumericValue $ move4CharactersToEnd n) `mod` 97 == 1

-- Automated test and helpers for incorrect ibans
genCountryCodeTouple :: Gen ([Char], Int)
genCountryCodeTouple = elements countrycodes

genNumbers :: Int -> Gen [Int]
genNumbers n = listOf (choose (0,9)) `suchThat` (\xs -> length xs == n)

--As can be seen in the main function, this generator is used not make invalid IBAN numbers. 
-- It has three components: 
-- 1. Choose a random country code, and country IBAN length from the given list.
-- 2. Generate an Int array based on the country IBAN length with digits (0-9)
-- 3. Return a string starting with the country code and the correct amount of random digits after. 
-- This generator will sometimes create valid IBANs, that result in a checksum of 1. While this is somewhat rare, it does occur. 
genInvalidIBAN :: Gen String
genInvalidIBAN = do 
    countryTouple <- genCountryCodeTouple >>= \x -> return x
    randomDigits <- genNumbers (snd countryTouple - 2)
    return (fst countryTouple ++ concatMap show randomDigits)

main :: IO () 
main = do 
    print "10 Correct example IBANs"
    print $ iban "BR1500000000000010932840814P2"
    print $ iban "BG18RZBB91550123456789"
    print $ iban "CR23015108410026012345"
    print $ iban "SV43ACAT00000000000000123123"
    print $ iban "XK051212012345678906"
    print $ iban "NO8330001234567"
    print $ iban "PK36SCBL0000001123456702"
    print $ iban "PS92PALS000000000400123456702"
    print $ iban "PL10105000997603123456789123"
    print $ iban "QA54QNBA000000000000693123456"
    print "====================================="
    print "Check for 100 invalid ibans, sometimes valid IBANs will occur."
    quickCheck $ forAll genInvalidIBAN $ not . iban

-- Time spent: 3h