module Exercise1 where

import Data.List
import LTS

-- Helpers
--getIOLTSStates : returns the whole list of states in a given OLTS. Since state list is the first element we extracted the first element of the tuple.
getIOLTSStates :: IOLTS -> [State]
getIOLTSStates (n, _, _, _, _) = n

--getIOLTSInputs : returns the whole list of input labels in a given OLTS. Since input label list is the second element we extracted the second element of the tuple.
getIOLTSInputs :: IOLTS -> [Label]
getIOLTSInputs (_, n, _, _, _) = n

--getIOLTSOutputs : returns the whole list of output labels in a given OLTS. Since output label list is the third element we extracted the third element of the tuple.
getIOLTSOutputs :: IOLTS -> [Label]
getIOLTSOutputs (_, _, n, _, _) = n

--getIOLTSTransitions :: returns the whole list of state transitions with the following labels in order to construct the hieararchy from the initial state to the leaves.
--Since transitions are placed as the fourth element we extracted the fourth element of the tuple.
getIOLTSTransitions :: IOLTS -> [LabeledTransition]
getIOLTSTransitions (_, _, _, n, _) = n

--getIOLTSStart : returns the initial state in a given OLTS. Since initial state is the last element we extracted the last element of the tuple.
getIOLTSStart :: IOLTS -> State
getIOLTSStart (_, _, _, _, n) = n

-- Properties
-- Here we have defined some properties which the createIOLTS function does not check where may lead to incorrect IOLTSs according to the definition of the Tretman Paper.

--Property 1 : Initial State must be an element of state list.
checkStartIsSubsetOfStates :: IOLTS -> Bool
checkStartIsSubsetOfStates iolts = getIOLTSStart iolts `elem` getIOLTSStates iolts

--Property 2 : State list must be non-empty.
checkStatesIsNotEmpty :: IOLTS -> Bool
checkStatesIsNotEmpty iolts = not (null (getIOLTSStates iolts))

--Property 3 : Input Labels and Output Labels must not intersect. If a label is present in input labels, it must not be present in output labels.
checkIOIntersectionIsNull :: IOLTS -> Bool
checkIOIntersectionIsNull iolts = all (\x -> x `notElem` getIOLTSOutputs iolts) (getIOLTSInputs iolts)

--Property 4 : The state transitions must be between defined states. That is given a state transition, the starting state and the destination state must be belong to the
--state list. 
checkTransitionsElementOfStates :: IOLTS -> Bool
checkTransitionsElementOfStates iolts = all (\(x, _, y) -> x `elem` getIOLTSStates iolts && y `elem` getIOLTSStates iolts) (getIOLTSTransitions iolts)

--validateLTS : Identifies if a given iolts function fits IOLTS format.Returns true if IOLTS fed is valid, and false when invalid.
--validateLTS considers the above 4 properties when evaluating createIOLTS function,therefore detects any IOLTS that violates.
validateLTS :: IOLTS -> Bool
validateLTS iolts = checkStartIsSubsetOfStates iolts && checkStatesIsNotEmpty iolts && checkIOIntersectionIsNull iolts && checkTransitionsElementOfStates iolts
