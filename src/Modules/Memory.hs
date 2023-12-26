{-# LANGUAGE FlexibleContexts #-}
module Modules.Memory where

import Control.Monad.State.Strict
import Clash.Prelude
import Modules.Instructions
import Language.Haskell.TH

type Value = Instr

type Addr  = Litr


romT :: MonadState (Vec 250 Instr) m => Addr -> m(Instr)
romT adr = do
  let idx = unpack adr :: Unsigned 11
  mem <- get
  return $ mem !! (idx)

memContents = fst $ shiftInAt0 (repeat Instr{opCode = 0b0, lit = 0b0} ) $(listToVecTH [Instr {opCode = 1, lit = 0},Instr {opCode = 1, lit = 0},Instr {opCode = 1, lit = 0},Instr {opCode = 1, lit = 1},Instr {opCode = 5, lit = 41},Instr {opCode = 1, lit = 0},Instr {opCode = 3, lit = 0},Instr {opCode = 1, lit = 0},Instr {opCode = 3, lit = 1},Instr {opCode = 1, lit = 0},Instr {opCode = 3, lit = 2},Instr {opCode = 7, lit = 0},Instr {opCode = 3, lit = 0},Instr {opCode = 2, lit = 0},Instr {opCode = 1, lit = 0},Instr {opCode = 22, lit = 0},Instr {opCode = 5, lit = 20},Instr {opCode = 1, lit = 0},Instr {opCode = 3, lit = 1},Instr {opCode = 4, lit = 22},Instr {opCode = 1, lit = 1},Instr {opCode = 3, lit = 1},Instr {opCode = 1, lit = 2},Instr {opCode = 3, lit = 2},Instr {opCode = 2, lit = 2},Instr {opCode = 2, lit = 0},Instr {opCode = 23, lit = 0},Instr {opCode = 29, lit = 0},Instr {opCode = 5, lit = 38},Instr {opCode = 2, lit = 1},Instr {opCode = 2, lit = 2},Instr {opCode = 18, lit = 0},Instr {opCode = 3, lit = 1},Instr {opCode = 2, lit = 2},Instr {opCode = 1, lit = 1},Instr {opCode = 16, lit = 0},Instr {opCode = 3, lit = 2},Instr {opCode = 4, lit = 24},Instr {opCode = 2, lit = 1},Instr {opCode = 31, lit = 0},Instr {opCode = 4, lit = 3},Instr {opCode = 6, lit = 0}])

memory inp = mealyS romT memContents inp


memoryM :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Addr)
  -> Signal System (Instr)
memoryM = exposeClockResetEnable memory
