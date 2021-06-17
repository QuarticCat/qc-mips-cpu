module MIPS.ALU where

import MIPS.ControlUnit
import MIPS.Prelude

type ALUOut =
  ( BitVector 32, -- result
    Bool, -- overflow flag
    Bool, -- zero flag
    Bool -- negative flag
  )

-- Check add overflow.
addOvf :: BitVector 32 -> BitVector 32 -> (BitVector 32, Bool)
addOvf a b =
  let c = a + b
   in (c, (a ! 31) == (b ! 31) && (a ! 31) /= (c ! 31))

-- Check sub overflow.
subOvf :: BitVector 32 -> BitVector 32 -> (BitVector 32, Bool)
subOvf a b =
  let c = a - b
   in (c, (a ! 31) /= (b ! 31) && (c ! 31) == (b ! 31))

{-# ANN
  arithUnit
  ( Synthesize
      { t_name = "arithUnit",
        t_inputs =
          ["OPERATION", "OPERAND_1", "OPERAND_2"],
        t_output =
          PortProduct "ALU" ["RESULT", "OVERFLOW", "ZERO", "NEG"]
      }
  )
  #-}
-- ALU. Handle most of the arithmetic works.
arithUnit :: ALUOp -> BitVector 32 -> BitVector 32 -> ALUOut
arithUnit op lhs rhs = (res, o, z, n)
  where
    (res, o, n) = arithUnit' op
    z = res == 0
    -- add and sub
    arithUnit' (ALUAdd isSigned) =
      let (res, ovf) = addOvf lhs rhs
       in (res, ovf && isSigned, bitToBool $ res ! 31)
    arithUnit' (ALUSub isSigned) =
      let (res, ovf) = subOvf lhs rhs
       in (res, ovf && isSigned, bitToBool $ res ! 31)
    -- bitwise operations
    arithUnit' ALUAnd = (lhs .&. rhs, False, False)
    arithUnit' ALUNor = (complement $ lhs .|. rhs, False, False)
    arithUnit' ALUOr = (lhs .|. rhs, False, False)
    arithUnit' ALUXor = (lhs `xor` rhs, False, False)
    -- set if less then
    arithUnit' (ALUSet isSigned) =
      let result =
            if isSigned
              then boolToBV $ (unpack lhs :: Signed 32) < unpack rhs
              else boolToBV $ (unpack lhs :: Unsigned 32) < unpack rhs
       in (result, False, False)
    -- shift left
    arithUnit' ALUShiftL =
      (lhs `shiftL` unpack (extend rhs), False, False)
    -- shift right (signed)
    arithUnit' (ALUShiftR True) =
      ( pack $ (unpack lhs :: Signed 32) `shiftR` unpack (extend rhs),
        False,
        False
      )
    -- shift right (unsigned)
    arithUnit' (ALUShiftR False) =
      (lhs `unsafeShiftR` unpack (extend rhs), False, False)
    -- no operation
    arithUnit' ALUNone = (0, False, False)
