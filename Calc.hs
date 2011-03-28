import RDParser
import ShuntingYard
import PostfixEval
import Tokenizer

calc :: String -> Int
calc exp = if parse exp  
           then postfixEval . shuntingYard . tokenize $ exp
           else error "Invalid expression."
