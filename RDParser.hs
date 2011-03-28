module RDParser ( parse ) where

import Data.Char

data Expected = Number | L_Oper | R_Oper | LParen | RParen
              deriving (Eq)

number :: String -> Bool
number [] = True
number (x:xs)
  | expect Number (x:xs) = operator xs
  | expect LParen (x:xs) = number   xs
  | otherwise = error "Expected a number or expression, got an operator.\n"
      
operator:: String -> Bool
operator [] = True
operator (x:xs)
  | expect L_Oper (x:xs) = number   xs
  | expect R_Oper (x:xs) = operator xs
  | expect RParen (x:xs) = operator xs 
  | otherwise = error "Expected an operator, got a number or expression.\n"

expect :: Expected -> String -> Bool
expect _ [] = True
expect typeOf (x:xs) 
  | typeOf == L_Oper  = x `elem` "+-*/%^"
  | typeOf == R_Oper  = x == '!'
  | typeOf == Number  = isDigit x
  | typeOf == RParen  = x == ')'
  | typeOf == LParen  = x == '(' && ')' `elem` xs

parse :: String -> Bool
parse (x:xs) = expect Number (x:xs) && operator xs
