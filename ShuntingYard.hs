module ShuntingYard ( shuntingYard ) where

import qualified Data.Char (isDigit)

data Assoc = AssocL | AssocR
           deriving Eq

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/%^!"

associativityOf :: Char -> Assoc
associativityOf x = if x `elem` "+-*/%"
                    then AssocL
                    else AssocR

precedenceOf :: Char -> Int
precedenceOf x
  | x `elem` "+-"    = 2
  | x `elem` "*/%"   = 3
  | x `elem` "^!"    = 4
  | otherwise        = 0

operatorActions :: String -> String -> String
operatorActions (x:xs) [] = shuntingYard_ xs [x]
operatorActions stmt@(x:xs) stack@(y:ys)
  | xIsAssocL && tokenPrec <= stackPrec = y : shuntingYard_ stmt ys
  | tokenPrec < stackPrec               = y : shuntingYard_ stmt ys
  | otherwise                           = shuntingYard_ xs $ x:stack
  where xIsAssocL  = associativityOf x == AssocL
        tokenPrec  = precedenceOf x
        stackPrec  = precedenceOf y
                           
stackOperations :: String -> String
stackOperations [] = []
stackOperations stack@(x:xs)
  | '(' `elem` stack = error "Unbalanced parentheses."
  | otherwise        = stack

shuntingYard_ :: String -> String -> String
shuntingYard_ []       ys       = stackOperations ys
shuntingYard_ (')':xs) []       = error "Unbalanced parens."
shuntingYard_ (')':xs) ('(':ys) = shuntingYard_ xs ys
shuntingYard_ (')':xs) (y:ys)   = y : shuntingYard_ (')':xs) ys
shuntingYard_ ('(':xs) ys       = shuntingYard_ xs $ '(':ys
shuntingYard_ (x:xs) stack
  | Data.Char.isDigit x = x : shuntingYard_ xs stack
  | isOperator x        = operatorActions (x:xs) stack
  | otherwise           = shuntingYard_ xs stack

shuntingYard :: String -> String
shuntingYard stmt = shuntingYard_ stmt []
