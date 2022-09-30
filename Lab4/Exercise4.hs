module Exercise4 where

import Data.List
import LTS

--state1 and state2 from labeled transitions (state1,label,state2)
--ioltsTrans ::IOLTS->  [(Int, Int)]

fst5 :: IOLTS -> [State]
fst5 (a, _, _, _, _) = a

label5 :: IOLTS -> [(State, Label, State)]
label5 (_, _, _, d, _) = d

snd2 :: (State, Label, State) -> Label
snd2 (_, a, _) = a

findInitial :: IOLTS -> State
findInitial (_, _, _, _, a) = a

--after "coin" coffeeModel1
after :: Label -> IOLTS -> [State]
after labl iolts = map (\(_, _, x) -> x) (filter (\(x, y, _) -> y == labl && x == findInitial iolts) (label5 iolts))

main :: IO ()
main =
  print $ after "coin" coffeeModel1
