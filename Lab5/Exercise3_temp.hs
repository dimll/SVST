import Mutation
import Test.QuickCheck
import Data.List
import MultiplicationTable
import Data.Maybe
import Debug.Trace

singleMutatorOfProp :: ([Integer] -> Gen[Integer]) -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Gen (Maybe Bool)
singleMutatorOfProp mut prop fut = mutate mut prop fut 10

multipleMutatorOfProp :: [([Integer] -> Gen[Integer])] -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Gen [(Maybe Bool)]
multipleMutatorOfProp [] prop fut = return []
multipleMutatorOfProp (x:xs) prop fut = do
    p <- singleMutatorOfProp x prop fut
    n <- multipleMutatorOfProp xs prop fut
    return (p : n)

multiplePropsAndMutators :: [([Integer] -> Gen[Integer])] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen [[(Maybe Bool)]]
multiplePropsAndMutators muts [] fut = return []
multiplePropsAndMutators muts (y:ys) fut = do
    ps <- multipleMutatorOfProp muts y fut
    n <- multiplePropsAndMutators muts ys fut
    return (ps:n)

transform