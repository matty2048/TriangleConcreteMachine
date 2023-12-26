{-# LANGUAGE NumericUnderscores #-}
 {-# OPTIONS_GHC -Wno-missing-signatures
 #-}
module Modules.Instructions where

import Clash.Prelude


type Litr = BitVector 11
type OpCode = BitVector 5
type ExternIO = BitVector 11

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
    lit    :: Litr
  }
  deriving (Generic, NFDataX, Show,Lift)
