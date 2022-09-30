module Exercise3 where

import Data.List
import Exercise1 (getIOLTSTransitions)
import Exercise2 (ltsGen)
import LTS
import Test.QuickCheck

-- Strip LabeledTransitions to Trace : It creates all possible paths that might occur given a IOLTS. The implementation is based on breadth first search since 
-- the number of possible paths are infinite due to the coescene.
straces :: IOLTS -> [Trace]
straces (ss, is, os, lts, start) = [] : filter (`isValidTrace` is) (map ltsToTrace (bfPaths lts []))

--ltsToTrace : is a helper function to help us strip each second element of a tuple to form a proper trace. When creating a trace we only use labels not the states.
--States only directs us on  breadth first search.What is included in the actual trace is the labels therefore we used this function.
ltsToTrace :: [LabeledTransition] -> Trace
ltsToTrace = map (\(_, x, _) -> x)

--isValidTrace filters out all invalid traces from the whole set of traces. We implemented this function because we assumed that deltas occur either before 
--inputs or after outputs and not in intermediary steps.
isValidTrace :: Trace -> [Label] -> Bool
isValidTrace t is = all (\(x, y) -> not (x `elem` is && y == delta)) (zip t (tail t ++ [head t]))

--bfPaths function creates breadth first searches and constructs the traces based on the searches.  We first check if the parent of a node is in the store.If so we append all its childs afterwards and return it. If not we simply 
-- add the element we are dealing with to the end of the store and continue. In order to eliminate the duplicates we apply union to the evaluated part of the list.
--We need to take in consideration that we are creating an infinite list. Therefore when evaluating we need to "take" only the portion we'd like to test.
bfPaths :: [LabeledTransition] -> [[LabeledTransition]] -> [[LabeledTransition]]
bfPaths [] store = store
bfPaths ((x, y, z) : xs) store = store `union` bfPaths (xs ++ [(x, delta, x), (x, y, z), (z, delta, z)]) (if parentsInStore store x then store ++ map (\xs -> xs ++ [(x, y, z)]) (getParents store x) else store ++ [[(x, y, z)]])

--getParents function is used to obtain the parent node of a node. If it is the root node which means no parent exists, return [], else return the parent.
getParents :: [[LabeledTransition]] -> State -> [[LabeledTransition]]
getParents ltss s
  | length (filter (\xs -> length xs /= 0) ltss) == 0 = []
  | otherwise = filter (\xs -> trd (last xs) == s) ltss


--parentsInStore function is used to check whether the parent of a node is present in the store. We have used this function to construct our breadth first search.
parentsInStore :: [[LabeledTransition]] -> State -> Bool
parentsInStore [[]] s = False
parentsInStore ltss s = not (null (getParents ltss s))

--ltsSnd is a helper function to strip the second element from the tuple. We used it to access the label given a state transition tuple.
ltsSnd :: (State, Label, State) -> Label
ltsSnd (_, x, _) = x

--trd is a helper function to strip the third element from the tuple. We used it to access the destination state given a state transition tuple.
trd :: (State, Label, State) -> State
trd (_, _, x) = x

--stracesGen : uses ltsGen which we created in exercise2 to generate multiple LTSs. We inputed these LTSs to our straces function to obtain all possible traces
--for each LTS.
stracesGen :: Gen [Trace]
stracesGen = do
  straces <$> ltsGen

--We wanted to check if our straces function works properly. To do this we wrote a QuickCheck property which checks whether all traces are initiated from the
--initial state mentioned in IOLTS. Assumes that the first LTS is the starting point.
checkIfStartPointsIsCorrect :: IOLTS -> Bool
checkIfStartPointsIsCorrect iolts = head (last (take 2 (straces iolts))) == ltsSnd (head (getIOLTSTransitions iolts))


main :: IO ()
main = do
  --Inside main we automated the quickCheck testing process for all generated LTSs to test the property we have written.
  quickCheck $ forAll ltsGen checkIfStartPointsIsCorrect
