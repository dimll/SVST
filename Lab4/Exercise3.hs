import Data.List
import Exercise1 (getIOLTSTransitions)
import Exercise2 (ltsGen)
import LTS
import Test.QuickCheck

-- Strip LabeledTransitions to Trace
straces :: IOLTS -> [Trace]
straces (ss, is, os, lts, start) = [] : filter (`isValidTrace` is) (map ltsToTrace (bfPaths lts []))

ltsToTrace :: [LabeledTransition] -> Trace
ltsToTrace = map (\(_, x, _) -> x)

isValidTrace :: Trace -> [Label] -> Bool
isValidTrace t is = all (\(x, y) -> not (x `elem` is && y == delta)) (zip t (tail t ++ [head t]))

bfPaths :: [LabeledTransition] -> [[LabeledTransition]] -> [[LabeledTransition]]
bfPaths [] store = store
bfPaths ((x, y, z) : xs) store = store `union` bfPaths (xs ++ [(x, delta, x), (x, y, z), (z, delta, z)]) (if parentsInStore store x then store ++ map (\xs -> xs ++ [(x, y, z)]) (getParents store x) else store ++ [[(x, y, z)]])

getParents :: [[LabeledTransition]] -> State -> [[LabeledTransition]]
getParents ltss s
  | length (filter (\xs -> length xs /= 0) ltss) == 0 = []
  | otherwise = filter (\xs -> trd (last xs) == s) ltss

parentsInStore :: [[LabeledTransition]] -> State -> Bool
parentsInStore [[]] s = False
parentsInStore ltss s = not (null (getParents ltss s))

ltsSnd :: (State, Label, State) -> Label
ltsSnd (_, x, _) = x

trd :: (State, Label, State) -> State
trd (_, _, x) = x

stracesGen :: Gen [Trace]
stracesGen = do
  straces <$> ltsGen

-- Test functions for straces
-- Assumes that the first LTS is the starting point
checkIfStartPointsIsCorrect :: IOLTS -> Bool
checkIfStartPointsIsCorrect iolts = head (last (take 2 (straces iolts))) == ltsSnd (head (getIOLTSTransitions iolts))

main :: IO ()
main = do
  quickCheck $ forAll ltsGen checkIfStartPointsIsCorrect
