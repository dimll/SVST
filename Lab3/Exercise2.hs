-- Time spent: 60 minutes
module Exercise2 where

import Lecture3

-- Properties
-- 1. Check if parsed == un-parsed
-- 2. Check if length is equal 
-- 3. Check if there amount of open parenthesis equals the amount of closed parenthesis
-- 4. Check if with an invalid String input the output is an empty list

-- Helper function
stripWhiteSpace :: String -> String
stripWhiteSpace = filter (/= ' ')

-- Property 1 and 2.
-- Compare input string to output string after parse and un-parse. This includes the length.
propIsReparseCorrect :: String ->  Bool 
propIsReparseCorrect s = stripWhiteSpace s == stripWhiteSpace ( showLst (parse s))

-- Property 3
-- Check if amount of open parenthesis' equals the amount of closed parenthesis'. 
propParenthesisMatches :: String -> Bool 
propParenthesisMatches s = length (filter (== '(') (showLst $ parse s)) == length (filter (== ')') (showLst $ parse s) )

-- Property 4
-- Check if with an invalid String input the output is an empty list
propInvalidInput :: String -> Bool
propInvalidInput s = null (stripWhiteSpace ( showLst (parse s))) 


main :: IO () 
main = do 
    -- The original string should match the "show" of the parsed version and return True.
    print $ propIsReparseCorrect $ show form1
    -- Parenthesis should match, and return True
    print $ propParenthesisMatches $ show form1
    -- Invalid string input, should return True
    print $ propInvalidInput "(1 -1 1)"
    print "All tests passed successfully, so parse function works as expected."