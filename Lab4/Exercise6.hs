-- Time spent 2 hours and failed... :(
import Control.Exception --Starting to lose control..
import LTS

perfectDoor :: IOLTS
perfectDoor = ([0 .. 6], ["close", "open", "lock", "unlock"], ["closed", "opened", "locked", "unlocked"], [(0, "close", 1), (1, "closed", 2), (2, "open", 3), (3, "opened", 0), (2, "lock", 4), (4, "locked", 5), (5, "unlock", 6), (6, "unlocked", 2)], 0)

{-
To solve this exercise we would need to create an adapter for the door implementations so that we could convert them into IOLTSs.

Essentially define this function:
adapter :: (State -> Label -> (State, Label)) -> IOLTS

Then use it like this:
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT iolts sut = (adapter sut) `ioco` iolts

Example:
testLTSAgainstSUT perfectDoor doorImpl1

Once we have the adapted IOLTS we can run the ioco function form ex 5 to see if they are ioco, and thereby valid.
-}