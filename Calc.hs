import ShuntingYard
import PostfixEval
import Tokenizer

calc ::  (Integral a) => String -> a
calc x = postfixEval . shuntingYard . tokenize x
