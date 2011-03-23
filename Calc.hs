import ShuntingYard
import RDParser
import PostfixEval
import Tokenizer

calc :: String -> Int
calc x = parse x && postfixEval . shuntingYard . tokenize x
