{-# LANGUAGE FlexibleContexts #-}
module Modules.Stack where

import Control.Monad.State.Strict
import Clash.Prelude
import Modules.Instructions

type Value = Litr
type Addr  = BitVector 8

data Op = Push Value | Pop | Load Addr | Store Addr | None
  deriving (Generic, NFDataX, Show)

data St =
  St {
    top :: Addr,
    mem :: Vec 100 Value,
    out :: Value
    }
    deriving (Generic, NFDataX)

data StkIn =
  StkIn {
    op :: Op
  }
  deriving(Generic, NFDataX,Show)

stackT (StkIn (Push val)) =
  do
    stk <- get
    let count = top stk
    let v = mem stk
    put(stk {top = count+1,mem = replace count val v, out = 0x0})
    return $ out stk
stackT (StkIn Pop) =
  do
    stk <- get
    let v = mem stk
    let count = top stk
    put(stk {top = count-1, mem = v,out = mem stk !! (count-1)})
    return $ out stk
stackT (StkIn (Store addr)) =
  do
    stk <- get
    let v = mem stk
    let count = top stk
    let val = v !! (count - 1)
    put(stk {top = count-1, mem = replace addr val v,out = 0x0})
    return $ out stk
stackT (StkIn (Load addr)) =
  do
    stk <- get
    let v = mem stk
    let count = top stk
    let val = v !! addr
    put(stk {top = count+1, mem = replace count val v, out = 0x0})
    return $ out stk
stackT (StkIn None) =
  do
    stk <- get
    put(stk {out = 0x0})
    return $ out stk

initialState = St 0b0 (repeat 0) 0b0


stack inp = mealyS stackT initialState inp


stackM :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (StkIn)
  -> Signal System (Value)
stackM = exposeClockResetEnable stack

