module Mutation where
import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Foldable

mutate :: ([Integer] -> Gen [Integer]) -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Integer -> Gen (Maybe Bool)
mutate mutator prop fut input = mutation >>= \mutant -> mutateOrNothing output mutant (Just <$> propertyExecutor prop mutation input)
        where output = fut input
              mutation = mutator output

mutateOrNothing :: [Integer] -> [Integer] -> Gen (Maybe Bool) -> Gen (Maybe Bool)
mutateOrNothing output mutant res | output == mutant = return Nothing
                                  | otherwise = res

propertyExecutor :: ([Integer] -> Integer -> Bool) -> Gen [Integer] -> Integer -> Gen Bool
propertyExecutor prop o x = o >>= \output -> return $ prop output x


-- Mutators
addElements :: [Integer] -> Gen [Integer]
addElements xs = do
  nums <- arbitrary :: Gen [Integer]
  nums2 <- arbitrary :: Gen [Integer]
  num <- arbitrary :: Gen Integer
  return $ num : nums++ xs ++ nums2


removeElements :: [Integer] -> Gen [Integer]
removeElements xs = choose (1, length xs - 1) >>= \x -> return $ take x xs

changeValueOfFirstElement :: [Integer] -> Gen[Integer]
changeValueOfFirstElement xs = do
  num <- arbitrary :: Gen Integer
  return $ num:tail(xs)

changeValueOfAnyElement :: [Integer] -> Gen[Integer]
changeValueOfAnyElement xs = do
  n <- choose (1, length xs - 1)
  let lst1 = take n xs
  let lst2 = drop ((length xs)-n) xs
  num <- (arbitrary :: Gen Integer) `suchThat` (\x -> x /= head xs * toInteger n)
  return (lst1 ++ [num] ++ lst2)

anyList :: [Integer] -> Gen[Integer]
anyList xs =arbitrary

reverseList :: [Integer] -> Gen[Integer]
reverseList xs = do 
  return (reverse xs)

--tested with: sample' $ mutate changeValueOfAnyElement MultiplicationTable.prop_sumIsTriangleNumberTimesInput  MultiplicationTable.multiplicationTable 1 