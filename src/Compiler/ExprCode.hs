module Compiler.ExprCode where

import           Compiler.ExpParser
import           Control.Applicative
import           Data.Char
import           Compiler.FunParser
import           Compiler.TAMmt
import           Compiler.ASTmt
import           Compiler.Env


binOpCode :: BinOperator -> [TAMInst]
binOpCode Addition = [ADD]
binOpCode Subtraction = [SUB]
binOpCode Multiplication = [MUL]
binOpCode Division = [DIV]
binOpCode Conjunction = [AND]
binOpCode Disjunction = [OR]
binOpCode LssOp = [LSS]
binOpCode GtrOp = [GRT]
binOpCode EqOp = [Compiler.TAMmt.EQ]
binOpCode LeqOp = [GRT,NOT]
binOpCode GeqOp = [LSS,NOT]
binOpCode NeqOp = [Compiler.TAMmt.EQ, NOT]


unOpCode :: UnOperator -> [TAMInst]
unOpCode Negation = [NEG]
unOpCode NegBool = [NOT]


-- res = b*t + !b*f
condOpCode :: VarEnv -> Expr -> Expr -> Expr -> [TAMInst]
condOpCode ve cond t f = genCodeExpr ve t ++ genCodeExpr ve cond ++ [LOADL 0] ++ [GRT] ++ [MUL] ++ genCodeExpr ve f ++ genCodeExpr ve cond ++ [LOADL 0] ++ [GRT] ++ [NOT] ++ [MUL] ++ [ADD]

--generates code transforms ast to TAMInst array
-- basically need to do post-order traversal easy in haskell :)
genCodeExpr :: VarEnv -> Expr -> [TAMInst]
genCodeExpr ve (BinOp op x y) = genCodeExpr ve x ++ genCodeExpr ve y ++  binOpCode op
genCodeExpr ve (UnOp op x) = genCodeExpr ve x ++ unOpCode op
genCodeExpr ve (LitInteger x) = [LOADL x]
genCodeExpr ve (Var x) = [LOAD $ address ve x]
genCodeExpr ve (Conditional cond t f) = condOpCode ve cond t f
