module PostfixEval ( postfixEval ) where

import qualified Data.Char (isDigit, digitToInt)

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial ( n - 1 )

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/%^!"

applyOper :: (Integral a) => Char -> a -> a -> a
applyOper x y z = 
  case x of
    '+' -> z + y
    '-' -> z - y
    '*' -> z * y
    '/' -> z `div` y
    '%' -> z `mod` y
    '^' -> z ^ y
    '!' -> factorial y
    
postfixEval' :: String -> [Int] -> Int
postfixEval' [] stack   = head stack
postfixEval' (x:xs) []  = postfixEval' xs [Data.Char.digitToInt x]
postfixEval' [x] (y:ys) = applyOper x y $ head ys
postfixEval' (x:xs) stack@(y:ys) 
  | Data.Char.isDigit x = postfixEval' xs $ Data.Char.digitToInt x : stack
  | isOperator x        = postfixEval' xs [applyOper x y $ head ys]  
  | otherwise           = postfixEval' xs stack

postfixEval :: String -> Int
postfixEval x = postfixEval' x []
