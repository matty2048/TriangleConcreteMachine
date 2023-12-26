module ExpEvaluator where
import              FunParser
import              ExpParser
import              Control.Applicative
import              Data.Char


binOpEv :: BinOperator -> Integer -> Integer -> Integer
binOpEv Addition       = (+)
binOpEv Subtraction    = (-)
binOpEv Multiplication = (*)
binOpEv Division       = div

unOpEv :: UnOperator -> Integer -> Integer
unOpEv Negation = negate

evaluate :: AST -> Integer
evaluate (LitInteger x)   = x
evaluate (BinOp op x y)   = binOpEv op (evaluate x) (evaluate y)
evaluate (UnOp op x)      = unOpEv op (evaluate x)
