{-# LANGUAGE LambdaCase #-}

module MIPS.DecodeModule where

import MIPS.ControlUnit
import MIPS.InstrDef
import MIPS.Prelude
import MIPS.RegisterFile

type DecodeOut =
  ( Maybe RegNo, -- rd
    MemOp, -- memory operation
    BranchFlag, -- branch flag
    ALUOp, -- ALU operation
    Maybe (BitVector 32), -- immediate value
    RegNo, -- rs
    RegVal, -- rs value
    RegNo, -- rt
    RegVal, -- rt value
    PCVal -- PC value
  )

-- Get read register numbers from instruction.
getReadRegs ::
  HiddenClockResetEnable dom =>
  Signal dom Instruction ->
  Signal dom (RegNo, RegNo)
getReadRegs =
  fmap $ \case
    NOP -> (0, 0)
    LW x _ _ -> (x, 0)
    SW x y _ -> (x, y)
    ADD x y _ -> (x, y)
    ADDU x y _ -> (x, y)
    ADDI x _ _ -> (x, 0)
    ADDIU x _ _ -> (x, 0)
    SUB x y _ -> (x, y)
    SUBU x y _ -> (x, y)
    AND x y _ -> (x, y)
    ANDI x _ _ -> (x, 0)
    NOR x y _ -> (x, y)
    OR x y _ -> (x, y)
    ORI x _ _ -> (x, 0)
    XOR x y _ -> (x, y)
    XORI x _ _ -> (x, 0)
    BEQ x y _ -> (x, y)
    BNE x y _ -> (x, y)
    SLT x y _ -> (x, y)
    SLL _ x _ -> (x, 0)
    SRL _ x _ -> (x, 0)
    SRA _ x _ -> (x, 0)
    SLLV x y _ -> (y, x)
    SRLV x y _ -> (y, x)
    SRAV x y _ -> (y, x)
    J _ -> (0, 0)
    JR x -> (x, 0)
    JAL _ -> (0, 0)

{-# ANN
  decodeModule
  ( Synthesize
      { t_name = "DecodeModule",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            "WRITE_REG",
            "STALL",
            "INSTRUCTION",
            "COUNTER"
          ],
        t_output =
          PortProduct
            "DM"
            [ "WRITE",
              "MEM",
              "BRANCH_FLAG",
              "ALU",
              "IMM",
              "RS",
              "RSV",
              "RT",
              "RTV",
              "COUNTER"
            ]
      }
  )
  #-}
-- Extract information from instruction.
decodeModule ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Maybe (RegNo, RegVal)) -> -- write pair
  Signal System Bool -> -- stall signal
  Signal System Instruction -> -- instruction
  Signal System PCVal -> -- PC value
  Signal System DecodeOut -- see above
decodeModule clk rst en writePair stall instr pc = bundle (w, m, b, a, i, rs, rsv, rt, rtv, pc')
  where
    -- get instruction and PC from state machine and update state
    (instr', pc') = stallMealyB (NOP, 0) clk rst en (stall, bundle (instr, pc))
    -- get read register numbers
    (rs, rt) = unbundle $ exposeClockResetEnable getReadRegs clk rst en instr'
    -- get all Control Unit outputs
    (w, m, b, a, i) = controlUnit clk rst en instr'
    -- get rs value and rt value from register file
    (rsv, rtv) = unbundle $ registerFile clk rst en $ bundle (rs, rt, writePair)
