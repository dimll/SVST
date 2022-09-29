module Ex1 where

import LTS
import Data.List

--checks if [States] is an empty list
isEmptyStates::[Int] -> Bool
isEmptyStates stateList = if stateList == [] then True else False

--string operations
split :: String -> [String]
split [] = [""]
split (c:cs) | c == ']'  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

subset lst1 lst2 = all (`elem` lst2) lst1

convert :: Int -> Int
convert str = fromEnum str - fromEnum '0'

strToint :: String -> [Int]
strToint str = map (convert) (map (fromEnum) (filter(`elem ` ['0','1','2','3','4','5','6','7','8','9']) str))

-- Remove punctuation from text String.
removePunc :: String -> String
removePunc xs = [x | x <- xs, not (x `elem` "[],.?!-:;\"\'")]

-- [States] from IOLTS
ioltsStates ::IOLTS-> [Int]
ioltsStates iolts =strToint (split(show (iolts)) !! 0)

-- q0 from IOLTS
ioltsQ0 ::IOLTS-> [Int]
ioltsQ0 iolts =strToint ((split(show (iolts)))!! (length((split(show (iolts))))-1))

--state1 and state2 from labeled transitions (state1,label,state2)
ioltsTrans ::IOLTS-> [Int]
ioltsTrans iolts =strToint (split(show (iolts)) !! 3)

-- input Labels, output Labels of iolts
ioltsInputLabels :: IOLTS -> String
ioltsInputLabels iolts = removePunc (split(show (iolts)) !! 1)

ioltsOutputLabels :: IOLTS -> String
ioltsOutputLabels iolts = removePunc (split(show (iolts)) !! 2)



--States must be non-empty
--q0=initial state must be an element of States
--in the labeled transitions the transition of states are all elements of States (state,label,state)
-- input labels and output labels can not be the same ==> (input labels) intersection (output labels)=[]
validateLTS :: IOLTS -> Bool 
validateLTS iolts = ((not$isEmptyStates(ioltsStates iolts)  && (((ioltsQ0 iolts)!!0) `elem` (ioltsStates iolts)))  && (subset (ioltsTrans iolts) (ioltsStates iolts))) && ((subset (ioltsInputLabels iolts)  (ioltsOutputLabels iolts))== False)

--I used valid examples from LTS.hs
--I generated invalid examples manually below:

-- Create Invalid LTS Examples:
--inValidcounterImpl :: IOLTS
--inValidcounterImpl = createIOLTS [(5, "?coin", 1), (2, "!tea", 3), (2, "!coffee", 4)]

main :: IO ()
main = putStrLn "VALIDATING LTS "
