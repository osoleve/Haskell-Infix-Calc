import ShuntingYard
import PostfixEval
import Tokenizer

calc :: String -> Int
calc = postfixEval . shuntingYard . tokenize
