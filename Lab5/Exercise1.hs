module Exercise1 where
    
import Test.QuickCheck
import Data.List
import Data.Maybe
import MultiplicationTable
import FitSpec
import Mutation

-- Add Elements:
-- It breaks property 1, because it changes the length
-- It breaks property 2, by changing the first element in the list but the same element can appear since the number is random
-- It breaks property 3, by changing the summatio of elements baes on the element generated randomly. If 0= it does not break
-- It breaks property 4, the sequence of elements are broken since the same element is added to the both start and end of the list
-- It breaks property 5, if the number generated modulo input not 0. It dependson the number generated.
-- We must note that there is a case that addElements will generate empty list [] so it will actually 
-- be identical with the original list. In that case mutate function will return Nothing. 
-- addElements is the strongest mutator since it covers all the 5 properties. 

-- Remove Elements:
-- It breaks property 1, because it changes the length
-- It does NOT break property 2, because element is removed from end of the list.
-- It breaks property 3, by changing the summation of elements baes on the element generated randomly. If 0= it does not break
-- It does NOT breaks property 4, the sequence of elements are still the same since we remove at the end of the list, it just with less elements.
-- It does NOT breaks property 5,the checked numbers are same, just the amount changes.
-- removeElements is weaker compared to the addElements mutator

-- Any List:
-- It breaks all of the properties. However it's a "silly" mutator because 
-- it just generates a random list 

-- Extra Mutators we have written:

-- Alters the value of the first element 
changeValueOfFirstElement :: [Integer] -> Gen[Integer]
changeValueOfFirstElement xs = do
  num <- arbitrary :: Gen Integer
  return $ num:tail(xs)

-- Alters the value of a random element 
changeValueOfAnyElement :: [Integer] -> Gen[Integer]
changeValueOfAnyElement xs = do
  n <- choose (1, length xs - 1)
  let lst1 = take n xs
  let lst2 = drop ((length xs)-n) xs
  num <- (arbitrary :: Gen Integer) `suchThat` (\x -> x /= head xs * toInteger n)
  return (lst1 ++ [num] ++ lst2)

-- Reverses the input list
reverseList :: [Integer] -> Gen[Integer]
reverseList xs = do 
  return (reverse xs)

   
