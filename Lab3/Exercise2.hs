-- Time spent: x minutes

import Lecture3

-- Properties
-- 1. Check if parsed == un-parsed
-- 2. Check if length is equal 
-- 3. Check if there amount of open parenthesis equals the amount of closed parenthesis

stripWhiteSpace :: String -> String
stripWhiteSpace = filter (/= ' ')

propIsReparseCorrect :: String ->  Bool 
propIsReparseCorrect s = stripWhiteSpace s == stripWhiteSpace ( showLst (parse s))

propParenthesisMatches :: String -> Bool 
propParenthesisMatches s = length (filter (== '(') s) == length (filter (== ')') s )

testPropIsReparseCorrect :: String -> Bool 
testPropIsReparseCorrect s = parse s --> propIsReparseCorrect s

testPropParenthesisMatches :: String -> Bool 
testPropParenthesisMatches s = parse s --> propParenthesisMatches s