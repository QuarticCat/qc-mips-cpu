module MIPS.ArithModule where

import Data.Maybe
import MIPS.ALU
import MIPS.ControlUnit
import MIPS.DecodeModule
import MIPS.ForwardUnit
import MIPS.Prelude

-- memory opeartion that carries write data
data MemOp'
  = MemLoad'
  | MemWrite' (BitVector 32)
  | MemNone'
  deriving (Generic, NFDataX)

type ArithOut =
  ( Maybe RegNo, -- write register number
    MemOp', -- memory operation
    BitVector 32, -- ALU result
    Maybe (Unsigned 32) -- branch target
  )

{-# ANN
  arithModule
  ( Synthesize
      { t_name = "ArithModule",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            "FW_0",
            "FW_1",
            "STALL",
            PortProduct
              "AM"
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
          ],
        t_output =
          PortProduct
            "AM"
            [ "WRITE_REG",
              "MEM_OP",
              "RESULT",
              "BRANCH_TARGET"
            ]
      }
  )
  #-}
-- Do a bunch of calculations for later stages. See below for details.
arithModule ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System ForwardInfo -> -- overridden rs value
  Signal System ForwardInfo -> -- overridden rt value
  Signal System Bool -> -- stall signal
  Signal System DecodeOut -> -- output of Decode Unit
  Signal System ArithOut -- see above
arithModule clk rst en exec load stall state = bundle (writeReg, memOp', res', branch')
  where
    -- get last output of Decode Module from state machine and update state
    clearState = (Nothing, MemNone, BranchNone, ALUNone, Nothing, 0, 0, 0, 0, 0)
    (writeReg, memOp, branchFlag, aluOp, imm, rs, rsv, rt, rtv, pc) =
      stallMealyB clearState clk rst en (stall, state)

    -- get overridden values from Forward Unit
    (check0, check1) = unbundle $ forwardUnit clk rst en exec load rs rt

    -- override values
    rsv' = (<|>) <$> check0 <*> (pure <$> rsv)
    rtv0 = (<|>) <$> check1 <*> (pure <$> rtv) -- be used by `lw` / `sw`
    rtv' = (<|>) <$> imm <*> rtv0

    -- get calculation result and flags from ALU
    (res, _, z, _) =
      unbundle $
        arithUnit <$> aluOp <*> (fromMaybe 0 <$> rsv') <*> (fromMaybe 0 <$> rtv')
    -- wrap memory opeartion
    memOp' = memSolver <$> memOp <*> (fromMaybe 0 <$> rtv0)
    -- check if we need to jump or not
    branch' = checkBranch <$> z <*> branchFlag <*> pc <*> imm <*> rsv'
    -- replace ALU result with PC if this is a jump instruction (for JAL)
    res' = jumpRes <$> branchFlag <*> res <*> pc

    memSolver MemWrite value = MemWrite' value
    memSolver MemLoad _ = MemLoad'
    memSolver _ _ = MemNone'

    checkBranch True (BranchEQ delta) pc _ _ = Just (pc + unpack delta) -- BEQ
    checkBranch False (BranchNE delta) pc _ _ = Just (pc + unpack delta) -- BNE
    checkBranch _ BranchJump _ (Just i) _ = Just (unpack i) -- J / JAL
    checkBranch _ BranchJump _ _ (Just i) = Just (unpack i `unsafeShiftR` 2) -- JR
    checkBranch _ _ _ _ _ = Nothing

    jumpRes BranchJump a b = pack b `unsafeShiftL` 2
    jumpRes _ a b = a
