module Compiler.CodeGeneration where

-- modules we have previously worked on
import Compiler.ASTmt
import Compiler.TAMmt
import Compiler.State
import Compiler.Env
import Compiler.ExprCode
import Compiler.ParseProgram
import Compiler.FunParser

-- compiling expressions
expCode :: VarEnv -> Expr -> [TAMInst]
expCode ve expr = genCodeExpr ve expr

-- generating labels
freshLabel :: ST Int LName
freshLabel = do n <- stState
                stUpdate (n+1)
                return ('#' : (show n))

beginEndCode :: VarEnv -> [Command] -> ST Int [TAMInst]
beginEndCode ve [x] = commCode ve x
beginEndCode ve (x:xs) = do
  code <- commCode ve x
  rest <- beginEndCode ve xs
  return $ code ++ rest

-- compiling commands
commCode :: VarEnv -> Command -> ST Int [TAMInst]
commCode ve (IfThenElse e c1 c2) =
  let te = expCode ve e in
  do
    tc1 <- commCode ve c1
    tc2 <- commCode ve c2
    l1 <- freshLabel
    l2 <- freshLabel
    return $ te ++ [JUMPIFZ l1] ++ tc1 ++ [JUMP l2] ++ [Label l1] ++ tc2 ++ [Label l2]
commCode ve (GetInt v) = return [GETINT, STORE (address ve v)]
-- v := "whatever you write in the terminal"
commCode ve (While e c) =
  let te = expCode ve e in
  do
    tc <- commCode ve c
    l1 <- freshLabel
    l2 <- freshLabel
    return $
      [Label l1] ++ te ++ [JUMPIFZ l2] ++ tc ++ [JUMP l1] ++ [Label l2]
commCode ve (Assignment id expr) =
  let te = expCode ve expr in
  do
    return $ te ++ [STORE (address ve id)]
commCode ve (PrintInt expr) =
  let te = expCode ve expr in
    do
      return $ te ++ [PUTINT]
commCode ve (BeginEnd comm) = beginEndCode ve comm


-- TODO: commCode ve (...) = ...

-- STEP 1: COMPILING DECLARATIONS

declCode :: Declaration -> ST (VarEnv,StkAddress) [TAMInst]
declCode (VarDecl v) =
  -- convention: var x; is the same as var x := 0;.
  -- if you want to throw an error when a variable is used that hasn't been initialised,
  -- you could use `type Stack = [Maybe MTInt]` (instead of `type Stack = [MTInt]`)
  do
    (ve,a) <- stState
    stUpdate ((v,a) : ve, a+1)
    return [LOADL 0]
declCode (VarInit v e) =
  do
    (ve,a) <- stState
    stUpdate ((v,a) : ve, a+1)
    return (expCode ve e)

declsCode :: [Declaration] -> ST (VarEnv,StkAddress) [TAMInst]
declsCode [] = return []
declsCode (d:ds) =
  do
    ts <- declCode d
    tds <- declsCode ds
    return (ts ++ tds)

getFromCom :: ST Int [TAMInst] -> [TAMInst]
getFromCom st = fst $ app st 0

getFromDecl :: ST (VarEnv,StkAddress) [TAMInst] -> ([TAMInst], (VarEnv,StkAddress))
getFromDecl st = app st ([],0)


-- todo: combine declsCode and commCode to compile TAM programs!

genProg :: Program -> [TAMInst]
genProg (LetIn decls com) = do  let decl = declsCode decls
                                let (instdecl, (varEnv, _)) = getFromDecl decl
                                let pro = commCode varEnv com
                                let instcom = getFromCom pro
                                instdecl ++ instcom ++ [HALT]
-- done: AST --------> TAM program

-- next: execute TAM programs

