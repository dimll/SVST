module Exercise5 where

import LTS
import Data.List
import Ex4

filterLabels :: IOLTS -> Label -> [Label]
filterLabels n l = map (\(_,x,_) -> x) (filter (\(x,_,_) -> x `elem` (after l n)) (label5 n))