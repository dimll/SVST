module Exercise5 where

import Data.List
import Exercise1 (getIOLTSInputs, getIOLTSTransitions)
import Exercise3 (straces)
import Exercise4 (after)
import LTS

--subset function finds whether a list is subset of another. It basically does A-(AnB). It takes the difference of a list A and the intersection of list A with list B.
subset :: [Label] -> [Label] -> Bool
subset ls1 ls2 = null (ls1 \\ (ls2 `intersect` ls1))

--getLabels 
getLabels :: IOLTS -> Label -> [Label]
getLabels n l = map (\(_, x, _) -> x) (filter (\(x, _, _) -> x `elem` after l n) (getIOLTSTransitions n))

--out function returns the state after implementing "after" function given a IOLTS and a label. 
out :: IOLTS -> Label -> [Label]
out iolts l = map (\x -> if x == "tau" then "delta" else x) (getLabels iolts l ++ replicate (length (after l iolts) - length (getLabels iolts l)) delta)

--ioco checks whether 2 models are in a ioco relation. 2 models are in ioco relation if one of them is a subset of another and if the both trace includes such a transition.
ioco :: IOLTS -> IOLTS -> Bool
ioco impl model = all (\x -> x `elem` getIOLTSInputs impl) (getIOLTSInputs model) && all (all (\x -> subset (impl `out` x) (model `out` x))) (take 15 $ straces model)

main :: IO ()
main = do
--In the below section of main we have tested our ioco function with the samples provided in LTS.h.  
  print "1: Should be True: "
  print $ coffeeImpl1 `ioco` coffeeModel1
  print "2: Should be False: "
  print $ coffeeImpl2 `ioco` coffeeModel2
  print "3: Should be True: "
  print $ coffeeImpl3 `ioco` coffeeModel3
  print "4: Should be False: "
  print $ coffeeImpl4 `ioco` coffeeModel4
  print "5: Should be False: "
  print $ coffeeImpl5 `ioco` coffeeModel5
  print "6: Should be True: "
  print $ coffeeImpl6 `ioco` coffeeModel6
