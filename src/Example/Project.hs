{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
module Example.Project (topEntity, plus) where

import Control.Monad.State.Strict
import Control.Monad
import Clash.Prelude
import Clash.Signal


import Modules.Stack
import Modules.Memory
import Modules.Instructions

-- | Add two numbers. Example:
--
-- >>> plus 3 5
-- 8
plus ::KnownNat a => Unsigned a -> Unsigned a -> Unsigned a
plus a b = a + b

double :: Unsigned 8 -> Unsigned 8
double a = 2 * a

{--
--counterT :: Unsigned 8 -> a -> (Unsigned 8, Unsigned 8)
counterT acc _ = (acc', o)
  where
    acc' = plus 1 acc
    o = acc
--}
counterTS :: MonadState (BitVector 11) m => p -> m (BitVector 11)
counterTS _ = do
  acc <- get
  put(acc + 1)
  return acc

multS :: MonadState (Unsigned 8) m => (Unsigned 8, Unsigned 8) -> m (Unsigned 8)
multS (x,y) = do
  acc <- get
  put(x * y)
  if(isMult2 x)
    then do
      return x
    else do
      return acc

isMult2 :: Unsigned 8 -> Bool
isMult2 x = ((x `mod` 0b10) == 0)

{--
counter inp = do
    let (i1)  =  mealySB counterTS 0b0 inp
    let out = mealySB multS 0b0 (i1,0b11)
    out
--}

data Delta =
  Delta {
    -- instruction
    instr   ::  Instr,
    -- input from stack
    stackIn :: Litr
  }

data Opt =
  Opt {
    -- mem address
    addr :: Litr,
    -- Stack Instruction
    stkInst :: StkIn,
    -- output
    opt :: Maybe Litr
  }
  deriving (Generic, NFDataX, Show)

data CB =
  CB{
    -- program counter
    pc :: Litr,
    -- number of things to pop
    fCount :: Unsigned 2,
    -- a register
    rA :: Litr,
    -- b register
    rB :: Litr
  }
  deriving (Generic, NFDataX, Show)
initState = CB 0x0 0 0 0

stackInstrDecode :: (Instr, ExternIO) -> (StkIn, Unsigned 2)
stackInstrDecode (i, io) = res
  where
    op = opCode i
    l = lit i
    res  = case op of 0b0_0001 -> (StkIn (Push l),0)
                      0b0_0010 -> (StkIn (Load (truncateB l)),0)
                      0b0_0011 -> (StkIn (Store (truncateB l)),0)
                      0b0_0101 -> (StkIn (Pop), 0b1)
                      0b0_0111 -> (StkIn (Push io),0)
                      x | op_ADD <= x && x <= op_EQ -> (StkIn (Pop),0b10)
                      y | op_NOT <= y && y <= op_PRINT -> (StkIn (Pop), 0b1)
                      _ -> (StkIn(None), 0)

--takes an instruction and rA and rB and returns the result of applying this instruction
execution :: Instr -> Signed (BitSize Litr) -> Signed (BitSize Litr) -> Signed (BitSize Litr)
execution inst rA rB
 | i == op_ADD = a + b
 | i == op_SUB = a - b
 | i == op_MUL = a * b
 | i == op_DIV = a `quot` b
 | i == op_NEG = -a
 | i == op_AND = (if a > 0 && b > 0 then 0b1 else 0b0)
 | i == op_OR = (if a > 0 || b > 0 then 0b1 else 0b0)
 | i == op_LSS = (if a < b then 0b1 else 0b0)
 | i == op_GRT = (if a > b then 0b1 else 0b0)
 | i == op_NOT = (if a > 0 then 0b0 else 0b1)
 | i == op_EQ  = (if(a == b) then 0b1 else 0b0)
 | otherwise = a
  where
    a  = rA
    b  = rB
    i  = opCode inst

stackInstGen :: OpCode -> Unsigned 2 -> Litr -> StkIn
stackInstGen op fCount l
  | fCount > 0 && op /= op_JMPIFZ= StkIn Pop
  | op /= op_JMPIFZ && op /= op_PRINT  = StkIn $ Push l
  | otherwise = StkIn None


controllerTS :: MonadState (CB) m => (ExternIO,Litr,Instr) -> m (Opt)
controllerTS (io,st,rm) = do
  curState   <- get
  let counter =  pc curState
  let inst    =  rm
  let stk     =  st
  let numPop  =  fCount curState
  if(numPop > 0) then do
    if (numPop == 2) then do
      let b = pack $ stk
      put(curState {fCount = numPop - 1, rB = b})
    else do
      let a = pack $ stk
      let newpc = if(opCode inst == op_JMPIFZ && a == 0) then lit inst else counter + 1
      put(curState {fCount = numPop - 1, rA = a, pc = newpc})
    curState <- get
    let res = execution inst (unpack $ rA curState) (unpack $ rB curState)
    let stkOp' = stackInstGen (opCode inst) (fCount curState) (pack res)
    let opt' = if (opCode inst == op_PRINT) then Just $ rA curState else Nothing
    return $ Opt (counter) (stkOp') (opt')
  else do
    let (stkOp',fCount' ) = stackInstrDecode (inst,io)
    let newpc = (if (opCode inst == op_JMP) then lit inst else if(fCount' > 0 || opCode inst == op_HALT) then pc curState else pc curState + 1)
    put(curState{pc = newpc, fCount = fCount'})
    return $ Opt (counter)  (stkOp') (Nothing)


cpu io = opt <$> out
  where
    out = mealyS (controllerTS) (initState) (bundle(io,stk1,instr1))
    instr1 =  (memory $ addr <$> out)
    stk1   =  (stack $ stkInst <$> out)


-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
topEntity :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (ExternIO)
  -> Signal System (Maybe Litr)
topEntity  i = exposeClockResetEnable cpu i
