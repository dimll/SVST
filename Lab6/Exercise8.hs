module Excerise8 where

import Lecture6
import Data.List
import Data.Char

instance Show Expr where
  show (I i) = show i
  show (V v) = v
  show (Add expr1 expr2) = show expr1 ++ " + " ++ show expr2
  show (Subtr expr1 expr2) = show expr1 ++ " - " ++ show expr2
  show (Mult expr1 expr2) = show expr1 ++ " * " ++ show expr2

instance Show Condition where
  show (Prp v) = show v
  show (Eq expr1 expr2) = show expr1 ++ " == " ++ show expr2
  show (Lt expr1 expr2) = show expr1 ++ " < " ++ show expr2
  show (Gt expr1 expr2) = show expr1 ++ " > " ++ show expr2
  show (Ng c) = "Neg " ++ show c
  show (Cj c) = concatMap (\x -> show x ++ " && ") (init c) ++ show (last c)
  show (Dj c) = concatMap (\x -> show x ++ " || ") (init c) ++ show (last c)

instance Show Statement where
  show (Ass var expr1) = var ++ " = " ++ show expr1 ++ "\n"
  show (Cond c s1 s2) = show c ++ show s1 ++ show s2
  show (Seq s) = concatMap (\x -> show x) s
  show (While c s) = "while (" ++ show c ++ ") {\n" ++ show s ++ "}\n"

-- Comments for read function
-- 1. Split output of show using lines function from Haskell. This will output a lists of Strings
-- 2. Use guards and elem to detect if specific operators are present in the string
-- 3. If the statement is "while", then use takeWhile until "}" has been found to create a list of strings that will represent the sequence
-- 4. Feed sequence into same read function
-- 5. If condition, wrap in correct condition and return read with expressions
-- 6. If expression return correct expression
-- #TouchMyFishFillet

main :: IO ()
main = do
    putStr (show fib)