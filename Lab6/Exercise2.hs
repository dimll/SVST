-- Time spent: 90 minutes
module Exercise2 where

import Data.List
import DynFlags (xFlags)
import Exercise1
import SetOrd
import Test.QuickCheck

-- Filter away any element that is in a but is not in b, return result as Set
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) b = Set $ filter (`inSet` b) a

-- Take all elements of a, add all elements of b not already in a, return as set
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = Set $ sort $ nub $ a ++ b

-- Remove all elements from a that exist in b, return as set
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) b = Set $ filter (\x -> not (x `inSet` b)) a

-- Length properties
-- Helper
setLength :: Set a -> Int
setLength (Set a) = length a

-- Check that length of intersection is less than or equal to the length of a
propIntersectionLength :: Ord a => Set a -> Set a -> Bool
propIntersectionLength a b = setLength (setIntersection a b) <= setLength a

-- Check that the length of the union of a and b is greather than or equal to a
propUnionLength :: Ord a => Set a -> Set a -> Bool
propUnionLength a b = setLength (setUnion a b) >= setLength a

-- Check that the length of the difference of a and b less than or equal to the length of a
propDifferenceLength :: Ord a => Set a -> Set a -> Bool
propDifferenceLength a b = setLength (setDifference a b) <= setLength a

-- Content properties
-- Check that every element of the intersection of a and b exists in both a and b
propIntersectionContent :: Ord a => Set a -> Set a -> Bool
propIntersectionContent a b = all (`inSet` a) x && all (`inSet` b) x
  where
    (Set x) = setIntersection a b

-- Check that every element of a and b exists in the union of a and b
propUnionContent :: Ord a => Set a -> Set a -> Bool
propUnionContent (Set a) (Set b) = all (`inSet` x) a && all (`inSet` x) b
  where
    x = setUnion (Set a) (Set b)

-- Check that every element of the difference between a and b exists in a
propDifferenceContent :: Ord a => Set a -> Set a -> Bool
propDifferenceContent a b = all (`inSet` a) x
  where
    (Set x) = setDifference a b

main :: IO ()
main = do
  set1 <- genSetWithoutQ
  set2 <- genSetWithoutQ
  print "Set 1"
  print set1
  print "Set 2"
  print set2

  print "Intersection tests"
  print $ propIntersectionLength set1 set2
  quickCheck $ forAll genSet $ \set1 -> forAll genSet $ \set2 -> propIntersectionLength set1 set2
  print $ propIntersectionContent set1 set2
  quickCheck $ forAll genSet $ \set1 -> forAll genSet $ \set2 -> propIntersectionContent set1 set2

  print "Union tests"
  print $ propUnionLength set1 set2
  quickCheck $ forAll genSet $ \set1 -> forAll genSet $ \set2 -> propUnionLength set1 set2
  print $ propUnionContent set1 set2
  quickCheck $ forAll genSet $ \set1 -> forAll genSet $ \set2 -> propUnionContent set1 set2

  print "Difference tests"
  print $ propDifferenceLength set1 set2
  quickCheck $ forAll genSet $ \set1 -> forAll genSet $ \set2 -> propDifferenceLength set1 set2
  print $ propDifferenceContent set1 set2
  quickCheck $ forAll genSet $ \set1 -> forAll genSet $ \set2 -> propDifferenceContent set1 set2