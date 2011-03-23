module Tokenizer ( tokenize ) where

import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Function (on)

tokenize :: String -> [String]
tokenize [] = []
tokenize x = let groupOn = groupBy . on (==) 
             in  groupOn isDigit x
