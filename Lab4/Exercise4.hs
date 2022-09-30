-- Time spent: 1 hour
module Exercise4 where

import Data.List
import LTS

--state1 and state2 from labeled transitions (state1,label,state2)
--ioltsTrans ::IOLTS->  [(Int, Int)]

--fst5 is a helper function to strip the state list from the given IOLTS.
fst5 :: IOLTS -> [State]
fst5 (a, _, _, _, _) = a

--label5 is a helper function to strip the state transitions from the given IOLTS.
label5 :: IOLTS -> [(State, Label, State)]
label5 (_, _, _, d, _) = d

--snd2 is a helper function to strip the label from the state transition.
snd2 :: (State, Label, State) -> Label
snd2 (_, a, _) = a

--findInitial is a helper function to strip the initial state from the given IOLTS.
findInitial :: IOLTS -> State
findInitial (_, _, _, _, a) = a

--Assuming the starting state is always q0 from the IOLTS, we find out the states we may arrive after implementing a given label for an IOLTS.
--For example if -- after "coin" coffeeModel1 -- is typed, the function will first search for label "coin" in the given IOLTS and when found from 
--all possible label matches, it will return all possible next states we may end up in. 
after :: Label -> IOLTS -> [State]
after labl iolts = map (\(_, _, x) -> x) (filter (\(x, y, _) -> y == labl && x == findInitial iolts) (label5 iolts))

main :: IO ()
main =
  print $ after "coin" coffeeModel1
