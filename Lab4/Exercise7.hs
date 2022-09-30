-- Time spent: 2.5h

{-# LANGUAGE TupleSections #-}

import Exercise1 (validateLTS)
import Exercise2 (ltsGen)
import LTS
import Test.QuickCheck

import Data.List

-- Final visualize function. 
-- The input is an IOLTS and the output 
-- is a visualization of the connected states
visualizeLTS :: IOLTS -> IO()
visualizeLTS x = createGraph (convert (ltsToIntegerLists $ ltsToLT x) 0)

-- Every output tuple contains an element
-- of the input inside list in the first position
-- and the index of the list that contains it 
-- as the second element.
-- Example to see how convert works 
-- Input = [[1],[2],[3,4]] and 0
-- Output = [(1,0),(2,1),(3,2),(4,2)]
convert :: [[Integer]] -> Int -> [(Integer, Int)]
convert [] start = []
convert (x : xs) start = map (,start) x ++ convert xs (start + 1)

-- Visual representation of states and their connections
-- using similar (oldschool hehe) technique with the linux 
-- files explorer. It should be noted that actions 
-- are not shown but only states and edges (meaning)
-- the connections between them.

-- This function takes the tree style path from IOLTS 
-- and outputs the visual representation at the console.
createGraph :: [(Integer,Int)] -> IO ()
createGraph [] = pure ()
createGraph (x:xs) = do 
    addElem (fst x) (snd x) 
    createGraph xs

-- Helper function for createGraph
addElem :: Integer -> Int -> IO()
addElem k pos = do
    let s =  take ((pos-1)*3) (repeat ' ')
    if (pos==0) then do
        print (show k)
    else do 
        print (s ++ "|" ++ "--" ++ show k)


-- Helper functions 
ltsToIntegerLists :: [LabeledTransition] -> [[Integer]]
ltsToIntegerLists lts = [1] : ltsLevelsToPoints (ltsToLevels lts []) 2

ltsToLT :: IOLTS -> [LabeledTransition]
ltsToLT (_,_,_,x,_) = x

ltsToLevels :: [LabeledTransition] -> [[LabeledTransition]] -> [[LabeledTransition]]
ltsToLevels [] store = store
ltsToLevels (x : xs) [] = ltsToLevels xs [[x]]
ltsToLevels (x : xs) store = ltsToLevels xs (if lstFst (last (last store)) == lstFst x then init store ++ [last store ++ [x]] else store ++ [[x]])

ltsLevelsToPoints :: [[LabeledTransition]] -> Integer -> [[Integer]]
ltsLevelsToPoints [] start = []
ltsLevelsToPoints (x : xs) s = [s .. (s - 1 + toInteger (length x))] : ltsLevelsToPoints xs (toInteger (length x) + s)

lstFst :: LabeledTransition -> State
lstFst (x, _, _) = x

lstTrd :: LabeledTransition -> State
lstTrd (_, _, x) = x



main :: IO ()
main = do
    -- Example with counterImpl
    putStrLn "Visualization with IOLTS counterImpl"
    visualizeLTS counterImpl

    -- Example with coffeeModel4
    putStrLn "Visualization with IOLTS coffeeModel4"
    visualizeLTS coffeeModel4

    -- Example with our custom IOLTS generator
    putStrLn "Visualization with random generated IOLTS"
    genericIOLTS <- generate ltsGen
    visualizeLTS genericIOLTS