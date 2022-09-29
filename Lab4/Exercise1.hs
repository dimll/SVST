module Exercise1 where

import Data.List
import LTS

-- Helpers
getIOLTSStates :: IOLTS -> [State]
getIOLTSStates (n, _, _, _, _) = n

getIOLTSInputs :: IOLTS -> [Label]
getIOLTSInputs (_, n, _, _, _) = n

getIOLTSOutputs :: IOLTS -> [Label]
getIOLTSOutputs (_, _, n, _, _) = n

getIOLTSTransitions :: IOLTS -> [LabeledTransition]
getIOLTSTransitions (_, _, _, n, _) = n

getIOLTSStart :: IOLTS -> State
getIOLTSStart (_, _, _, _, n) = n

-- Properties
checkStartIsSubsetOfStates :: IOLTS -> Bool
checkStartIsSubsetOfStates iolts = getIOLTSStart iolts `elem` getIOLTSStates iolts

checkStatesIsNotEmpty :: IOLTS -> Bool
checkStatesIsNotEmpty iolts = not (null (getIOLTSStates iolts))

checkIOIntersectionIsNull :: IOLTS -> Bool
checkIOIntersectionIsNull iolts = all (\x -> x `notElem` getIOLTSOutputs iolts) (getIOLTSInputs iolts)

checkTransitionsElementOfStates :: IOLTS -> Bool
checkTransitionsElementOfStates iolts = all (\(x, _, y) -> x `elem` getIOLTSStates iolts && y `elem` getIOLTSStates iolts) (getIOLTSTransitions iolts)

validateLTS :: IOLTS -> Bool
validateLTS iolts = checkStartIsSubsetOfStates iolts && checkStatesIsNotEmpty iolts && checkIOIntersectionIsNull iolts && checkTransitionsElementOfStates iolts
