{-# LANGUAGE NumericUnderscores #-}

module Compiler.TAMmt where

import Prelude hiding (EQ)
import Data.List
import Compiler.State
import Debug.Trace


--import Modules.Instructions

{- During the execution, the TAM consists of:
   * the program (list of TAM instructions)
   * program counter ("at what instruction are we?")
   * stack (memory)
-}

type MTInt = Int
type StkAddress = Int
type LName = String

data TAMInst
  = HALT             -- stops execution
  -- Stack Operations
  | LOADL MTInt      -- push Integer into the stack
  | LOAD StkAddress  -- push value in address location to top of the stack
  | STORE StkAddress -- pop the top of the stak and write the value at the address
  -- Input/Output
  | GETINT           -- reads an integer and push it on the stack
  | PUTINT           -- pops the top of the stack and prints it
  -- Labels and jumps
  | Label LName      -- Marks a point in the program we can jump to
  | JUMP LName       -- Jumps to point in the program with given label
  | JUMPIFZ LName    -- Pops the top of the stack and jumps if it is zero
  -- Arithmetic operations
  | ADD           -- adds two top values in the stack
  | SUB           -- subtract second element of stack from top
  | MUL           -- multiplies top values in the stack
  | DIV           -- divides the second value by the top (integer division)
  | NEG           -- negates the top of the stack
  -- Boolean operations
  | AND           -- Boolean conjunction (non-zero values are True)
  | OR            -- Boolean disjunction
  | NOT           -- Boolean negation
  -- Relational operations
  | LSS           -- order operation <
  | GRT           -- order operation >
  | EQ           -- equality operator
  deriving (Eq,Show)

type Counter   = Int

type OpCode = Int
op_NULL   :: OpCode = 0b0_0000
op_PUSH   :: OpCode = 0b0_0001
op_LOAD   :: OpCode = 0b0_0010
op_STORE  :: OpCode = 0b0_0011
op_JMP    :: OpCode = 0b0_0100
op_JMPIFZ :: OpCode = 0b0_0101
op_HALT   :: OpCode = 0b0_0110
op_READ   :: OpCode = 0b0_0111
op_ADD    :: OpCode = 0b1_0000
op_SUB    :: OpCode = 0b1_0001
op_MUL    :: OpCode = 0b1_0010
op_DIV    :: OpCode = 0b1_0011
op_AND    :: OpCode = 0b1_0100
op_OR     :: OpCode = 0b1_0101
op_LSS    :: OpCode = 0b1_0110
op_GRT    :: OpCode = 0b1_0111
op_EQ     :: OpCode = 0b1_1000
op_NOT    :: OpCode = 0b1_1101
op_NEG    :: OpCode = 0b1_1110
op_PRINT  :: OpCode = 0b1_1111

data Instr =
  Instr {
    opCode :: OpCode,
    lit    :: Int
  }
  deriving (Show)



labelInstr :: TAMInst -> TAMInst -> (TAMInst, LName)
labelInstr inst1 inst2 =
  case inst1 of
    Label x -> (inst2, x)
    _ -> (inst2, "")

labelInstrs :: [TAMInst] -> [(TAMInst,LName)]
labelInstrs inst = zipWith labelInstr ([HALT] ++ inst) inst

isLabel :: TAMInst -> Bool
isLabel (Label x) = False
isLabel _ = True

translate :: [LName] -> TAMInst -> Instr
translate arr (JUMP v) = o
  where
    addr = case elemIndex v (arr) of
                Nothing -> trace ("called with " ++ show arr) (error ("could not find label " ++ v ++ "\n"))
                Just n -> n
    o = Instr op_JMP (fromIntegral addr)
translate arr (JUMPIFZ v) = o
  where
    addr = case elemIndex v (arr) of
                Nothing -> trace ("called with " ++ show arr) (error ("could not find label " ++ v ++ "\n"))
                Just n -> n
    o = Instr op_JMPIFZ (fromIntegral addr)
translate arr (LOADL l) = Instr op_PUSH (fromIntegral l)
translate arr (LOAD addr) = Instr op_LOAD (fromIntegral addr)
translate arr (STORE addr) = Instr op_STORE (fromIntegral addr)
translate arr ADD = Instr op_ADD 0b0
translate arr SUB = Instr op_SUB 0b0
translate arr MUL = Instr op_MUL 0b0
translate arr DIV = Instr op_DIV 0b0
translate arr NEG = Instr op_NEG 0b0
translate arr AND = Instr op_AND 0b0
translate arr OR = Instr op_OR 0b0
translate arr NOT = Instr op_NOT 0b0
translate arr LSS = Instr op_LSS 0b0
translate arr GRT = Instr op_GRT 0b0
translate arr EQ = Instr op_EQ 0b0
translate arr PUTINT = Instr op_PRINT 0b0
translate arr GETINT = Instr op_READ 0b0
translate arr HALT = Instr op_HALT 0b0
translate arr z = error ("couldn't find thing" ++ show z)

assemble :: [TAMInst] -> [Instr]
assemble i = out
  where
    labeled =  unzip $ filter (\x -> isLabel $ fst x) (labelInstrs i)
    labels   = trace (show labeled) (snd labeled)
    processed = fst labeled
    out = fmap (translate labels) processed
-- writing out a TAM program
writeTAM :: [TAMInst] -> String
writeTAM = foldl (\s inst -> s ++  (show inst) ++ "\n") ""


parseTAM :: String -> [TAMInst]
parseTAM = pTAM . words where
  pTAM ("LOADL":x:src) = LOADL (read x) : pTAM src
  pTAM ("LOAD":x:src) = LOAD (read x) : pTAM src
  pTAM ("STORE":x:src) = STORE (read x) : pTAM src
  pTAM ("GETINT":src) = GETINT : pTAM src
  pTAM ("PUTINT":src) = PUTINT : pTAM src
  pTAM ("Label":x:src) = Label (read x) : pTAM src
  pTAM ("JUMP":x:src) = JUMP (read x) : pTAM src
  pTAM ("JUMPIFZ":x:src) = JUMPIFZ (read x) : pTAM src
  pTAM ("AND":src) = AND : pTAM src
  pTAM ("OR":src) = OR : pTAM src
  pTAM ("NOT":src) = NOT : pTAM src
  pTAM ("LSS":src) = LSS : pTAM src
  pTAM ("GRT":src) = GRT : pTAM src
  pTAM ("EQ":src) = EQ : pTAM src
  pTAM ("ADD":src) = ADD : pTAM src
  pTAM ("SUB":src) = SUB : pTAM src
  pTAM ("MUL":src) = MUL : pTAM src
  pTAM ("DIV":src) = DIV : pTAM src
  pTAM ("NEG":src) = NEG : pTAM src
  pTAM ("HALT":src) = HALT : pTAM src
  pTAM _ = []


-- find label inside program:
lCounter :: LName -> [TAMInst] -> Counter
lCounter l tam = case elemIndex (Label l) tam of
                   Nothing -> error ("could not find label " ++ l)
                   Just n -> n

data TAMState = TAMState {
  tsCode :: [TAMInst],
  tsCounter :: Counter,
  tsStack :: [MTInt]
}
  deriving (Eq,Show)

initTS :: [TAMInst] -> TAMState
initTS tam = TAMState {tsCode = tam, tsCounter = 0, tsStack = []}

setCounter :: Counter -> TAMState -> TAMState
setCounter i ts = ts {tsCounter = i}

incrCounter :: TAMState -> TAMState
incrCounter ts =
  let i = tsCounter ts
  in  ts {tsCounter = i+1}
  -- or:  in (setCounter (i+1) ts)

tsPop :: TAMState -> (MTInt,TAMState)
tsPop ts =
  let (x:xs) = tsStack ts
  in (x , ts {tsStack = xs})

tsPush :: MTInt -> TAMState -> TAMState
tsPush i ts =
  let
    stk = tsStack ts
    newstk = [i] ++ stk
  in
    ts {tsStack = newstk}

address2index :: [MTInt] -> StkAddress -> Int
address2index stk a = length stk - a - 1

tsRead :: StkAddress -> TAMState -> MTInt
tsRead a ts =
  let
    stk = tsStack ts
    idx = address2index stk a
  in
    stk !! idx


tsWrite :: StkAddress -> MTInt -> TAMState -> TAMState
tsWrite a x ts =
  let
    stk = tsStack ts
    (stk1,stk2) = splitAt (address2index stk a) stk
    newstk = stk1 ++ [ x ] ++ (tail stk2)
  in
    ts {tsStack = newstk}

getInt :: TAMState -> IO MTInt
getInt _ = do l <- getLine
              let num = (read l:: MTInt)
              return num

-- goal:
execute :: TAMInst -> TAMState -> IO TAMState
execute (JUMP l) ts =
  do let n = lCounter l (tsCode ts)
     return $ setCounter n ts
execute (STORE a) ts =
  -- strategy:
  -- 1. pop x (remove top entry x from stack). Function tsPop above.
  -- 2. save x to cell with address a. Function tsWrite above.
  -- 3. increase the counter by 1. Function 'incrCounter' above.
  do let (x,ts1) = tsPop ts
     let ts2 = tsWrite a x ts1
     let ts3 = incrCounter ts2
     return ts3
execute (Label _) ts = do return $ incrCounter ts
execute (LOADL l) ts =
  do let ts1 = tsPush l ts
     let ts2 = incrCounter ts1
     return ts2
execute (LOAD i) ts =
  do let val = tsRead i ts
     let ts1 = tsPush val ts
     let ts2 = incrCounter ts1
     return ts2
execute (GETINT) ts =
  do
     num <- getInt ts
     let ts1 = tsPush num ts
     return $ incrCounter ts1
execute (PUTINT) ts =
  do
    let (x,ts1) = tsPop ts
    putStrLn $ show x
    return $ incrCounter ts1
execute (JUMPIFZ l) ts =
  do
    let (x,ts1) = tsPop ts
    if (x == 0) then do
      let n = lCounter l (tsCode ts1)
      return $ setCounter n ts1
    else do
      return $ incrCounter ts1
execute (ADD) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let ts3 = tsPush (x + y) ts2
    return $ incrCounter ts3
execute (SUB) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let ts3 = tsPush (y - x) ts2
    return $ incrCounter ts3
execute (MUL) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let ts3 = tsPush (y * x) ts2
    return $ incrCounter ts3
execute (DIV) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let ts3 = tsPush (y `div` x) ts2
    return $ incrCounter ts3
execute (NEG) ts =
  do
    let (x,ts1) = tsPop ts
    let ts2 = tsPush (-x) ts1
    return $ incrCounter ts2
execute (AND) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let b = (y > 0) && (x > 0)
    let ts3 = tsPush (if b then 1 else 0) ts2
    return $ incrCounter ts3
execute (OR) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let b = (y > 0) || (x > 0)
    let ts3 = tsPush (if b then 1 else 0) ts2
    return $ incrCounter ts3
execute (NOT) ts =
  do
    let (x,ts1) = tsPop ts
    let v = if (x == 0) then 1 else 0
    let ts2 = tsPush v ts1
    return $ incrCounter ts2
execute (GRT) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let v = if (y > x) then 1 else 0
    let ts3 = tsPush v ts2
    return $ incrCounter ts3
execute (LSS) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let v = if (y < x) then 1 else 0
    let ts3 = tsPush v ts2
    return $ incrCounter ts3
execute (EQ) ts =
  do
    let (x,ts1) = tsPop ts
    let (y,ts2) = tsPop ts1
    let v = if (y == x) then 1 else 0
    let ts3 = tsPush v ts2
    return $ incrCounter ts3


-- run till Halt
executeAll :: TAMState -> IO TAMState
executeAll state = do let code = tsCode state
                      let count = tsCounter state
                      if(count < (length code)) then do
                        let instr = code !! count
                        if(instr == HALT) then return state
                        else do
                          newstate <- execute instr state
                          executeAll newstate
                      else do return state

run :: [TAMInst] -> IO TAMState
run insts = do let st = initTS insts
               executeAll st

-- alternative:
executeS :: TAMInst -> ST TAMState ()
executeS (STORE a) = do
  x <- popS
  writeAddrS a x
  incrCountS

popS :: ST TAMState MTInt
popS = do
  s <- stState
  let stk = tsStack s
  stkUpdateS (tail stk)
  return (head stk)

-- MTInt -> TAMState -> TAMState
pushS :: MTInt -> ST TAMState ()
pushS = undefined

findLabelS :: LName -> ST TAMState Counter
findLabelS l = do
  s <- stState
  return (lCounter l (tsCode s))

stkUpdateS :: [MTInt] -> ST TAMState ()
stkUpdateS stk = do
  s <- stState
  stUpdate (s{tsStack = stk})

writeAddrS :: StkAddress -> MTInt -> ST TAMState ()
writeAddrS a x = do
  s <- stState
  stUpdate (tsWrite a x s)

countUpdateS :: Counter -> ST TAMState ()
countUpdateS c = do
  s <- stState
  stUpdate (s{tsCounter = c})

incrCountS :: ST TAMState ()
incrCountS = do
  s <- stState
  let c = tsCounter s
  countUpdateS (c+1)
