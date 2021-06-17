{-# LANGUAGE LambdaCase #-}

module MIPS.ControlUnit where

import MIPS.InstrDef
import MIPS.Prelude

-- memory operation
data MemOp
  = MemLoad
  | MemWrite
  | MemNone
  deriving (Generic, NFDataX)

-- branch flag
data BranchFlag
  = BranchJump
  | BranchEQ (BitVector 32) -- target address
  | BranchNE (BitVector 32) -- target address
  | BranchNone
  deriving (Generic, NFDataX)

-- ALU operation
data ALUOp
  = ALUAdd Bool -- signedness
  | ALUSub Bool -- signedness
  | ALUAnd
  | ALUNor
  | ALUOr
  | ALUXor
  | ALUSet Bool -- signedness
  | ALUShiftL
  | ALUShiftR Bool -- signedness
  | ALUNone
  deriving (Generic, NFDataX)

-- Get write register number from instruction.
getWriteReg ::
  HiddenClockResetEnable dom =>
  Signal dom Instruction ->
  Signal dom (Maybe RegNo)
getWriteReg =
  fmap $ \case
    LW _ rt _ -> Just rt
    ADD _ _ rd -> Just rd
    ADDU _ _ rd -> Just rd
    ADDI _ rt _ -> Just rt
    ADDIU _ rt _ -> Just rt
    SUB _ _ rd -> Just rd
    SUBU _ _ rd -> Just rd
    AND _ _ rd -> Just rd
    ANDI _ rt _ -> Just rt
    NOR _ _ rd -> Just rd
    OR _ _ rd -> Just rd
    ORI _ rt _ -> Just rt
    XOR _ _ rd -> Just rd
    XORI _ rt _ -> Just rt
    SLT _ _ rd -> Just rd
    SLL rd _ _ -> Just rd
    SRL rd _ _ -> Just rd
    SRA rd _ _ -> Just rd
    SLLV _ _ rd -> Just rd
    SRLV _ _ rd -> Just rd
    SRAV _ _ rd -> Just rd
    JAL _ -> Just 31
    _ -> Nothing

-- Get memory operation from instruction.
getMemOp ::
  HiddenClockResetEnable dom =>
  Signal dom Instruction ->
  Signal dom MemOp
getMemOp =
  fmap $ \case
    LW {} -> MemLoad
    SW {} -> MemWrite
    _ -> MemNone

-- Get branch flag from instruction.
getBranchFlag ::
  HiddenClockResetEnable dom =>
  Signal dom Instruction ->
  Signal dom BranchFlag
getBranchFlag =
  fmap $ \case
    BEQ _ _ x -> BranchEQ (pack $ extend x)
    BNE _ _ x -> BranchNE (pack $ extend x)
    J _ -> BranchJump
    JR _ -> BranchJump
    JAL _ -> BranchJump
    _ -> BranchNone

-- Get ALU operation from instruction.
getALUOp ::
  HiddenClockResetEnable dom =>
  Signal dom Instruction ->
  Signal dom ALUOp
getALUOp =
  fmap $ \case
    LW {} -> ALUAdd True
    SW {} -> ALUAdd True
    ADD {} -> ALUAdd True
    ADDU {} -> ALUAdd False
    ADDI {} -> ALUAdd True
    ADDIU {} -> ALUAdd False
    SUB {} -> ALUSub True
    SUBU {} -> ALUSub False
    AND {} -> ALUAnd
    ANDI {} -> ALUAnd
    NOR {} -> ALUNor
    OR {} -> ALUOr
    ORI {} -> ALUOr
    XOR {} -> ALUXor
    XORI {} -> ALUXor
    BEQ {} -> ALUXor
    BNE {} -> ALUXor
    SLT {} -> ALUSet True
    SLL {} -> ALUShiftL
    SLLV {} -> ALUShiftL
    SRL {} -> ALUShiftR False
    SRLV {} -> ALUShiftR False
    SRA {} -> ALUShiftR True
    SRAV {} -> ALUShiftR True
    J _ -> ALUOr
    JR _ -> ALUOr
    JAL _ -> ALUOr
    _ -> ALUNone

-- Get immediate value from instruction.
getImmVal ::
  HiddenClockResetEnable dom =>
  Signal dom Instruction ->
  Signal dom (Maybe (BitVector 32))
getImmVal =
  fmap $ \case
    ADDI _ _ x -> Just (pack $ extend x)
    ADDIU _ _ x -> Just (pack $ extend x)
    ANDI _ _ x -> Just (extend x)
    ORI _ _ x -> Just (extend x)
    XORI _ _ x -> Just (extend x)
    LW _ _ x -> Just (pack $ extend x)
    SW _ _ x -> Just (pack $ extend x)
    SLL _ _ x -> Just (pack $ extend x)
    SRL _ _ x -> Just (pack $ extend x)
    SRA _ _ x -> Just (pack $ extend x)
    J x -> Just (pack $ extend x)
    JAL x -> Just (pack $ extend x)
    _ -> Nothing

{-# ANN
  controlUnit
  ( Synthesize
      { t_name = "ControlUnit",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            "Instruction"
          ],
        t_output =
          PortProduct
            "CTL"
            [ "WRITE",
              "MEM",
              "BRANCH_FLAG",
              "ALU",
              "IMM"
            ]
      }
  )
  #-}
-- Combine all the information above.
controlUnit ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Instruction -> -- instruction
  ( Signal System (Maybe RegNo), -- all the information above
    Signal System MemOp,
    Signal System BranchFlag,
    Signal System ALUOp,
    Signal System (Maybe (BitVector 32))
  )
controlUnit =
  exposeClockResetEnable $
    (,,,,)
      <$> getWriteReg
      <*> getMemOp
      <*> getBranchFlag
      <*> getALUOp
      <*> getImmVal
