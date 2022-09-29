import Exercise1 (validateLTS)
import LTS
import Test.QuickCheck

import Data.List


ioltsEx :: IOLTS
ioltsEx = counterImpl

pos1 :: IOLTS -> [State]
pos1 (a,_,_,_,_)=a

testCase :: [(Integer, Integer)]
testCase = [(1,2),(2,4),(2,3)]

printTest :: [(Integer, Int)]
printTest = [(2,0),(3,1),(4,1),(5,2)]

searchEl :: [(Integer, Integer)] -> Integer -> [Integer]
searchEl [] k = []
searchEl (x:xs) k 
    | fst x == k = (snd x) : searchEl xs k
    | otherwise = searchEl xs k

addElem :: Integer -> Int -> IO()
addElem k pos = do
    let s =  take ((pos-1)*3) (repeat ' ')
    if (pos==0) then do
        print (show k)
    else do 
        print (s ++ "|" ++ "--" ++ show k)

createGraph :: [(Integer,Int)] -> IO ()
createGraph [] = pure ()
createGraph (x:xs) = do 
    addElem (fst x) (snd x) 
    createGraph xs

main :: IO ()
main = do
    createGraph printTest