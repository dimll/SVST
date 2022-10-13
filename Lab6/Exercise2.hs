module Exercise2 where

import Data.List
import SetOrd

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) b = Set $ filter (`inSet` b) a

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = Set $ a ++ filter (`notElem` a) b

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) b = Set $ filter (\x -> not (x `inSet` b)) a

-- Length properties
setLength :: Set a -> Int
setLength (Set a) = length a

propIntersectionLength :: Ord a => Set a -> Set a -> Bool
propIntersectionLength a b = setLength (setIntersection a b) <= setLength a

propUnionLength :: Ord a => Set a -> Set a -> Bool
propUnionLength a b = setLength (setUnion a b) >= setLength a

propDifferenceLength :: Ord a => Set a -> Set a -> Bool
propDifferenceLength a b = setLength (setDifference a b) <= setLength a