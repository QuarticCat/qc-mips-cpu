module MIPS.MemModule where

import MIPS.ArithModule
import MIPS.ControlUnit
import MIPS.ForwardUnit
import MIPS.Prelude
import MIPS.RAM

type MemOut =
  ( Maybe (Unsigned 32), -- branchanch target
    Maybe (RegNo, RegVal) -- write pair
  )

{-# ANN
  memModule
  ( Synthesize
      { t_name = "MemModule",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            PortProduct
              "MMI"
              [ "WRITE_REG",
                "MEM_OP",
                "RESULT",
                "BRANCH_TARGET"
              ],
            "STALL"
          ],
        t_output =
          PortProduct "MMO" ["BRANCH", "WRITE_PAIR"]
      }
  )
  #-}
-- Write data into main memory and read data from main memory.
memModule ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System ArithOut -> -- output of Arith Module
  Signal System Bool -> -- stall signal
  Signal System MemOut -- see above
memModule clk rst en arithOut stall = check <$> stall <*> bundle (branch, writePair)
  where
    -- get last output of Arith Module from state machine and update state
    (writeReg, memOpPrev, aluOut, branch) =
      stallMealyB (Nothing, MemNone', 0, Nothing) clk rst en (stall, arithOut)

    -- extract memory operation from current output of Arith Module
    memOp = (\(_, x, _, _) -> x) <$> arithOut
    memAddr = (\(_, _, x, _) -> x `unsafeShiftR` 2) <$> arithOut

    -- get write pair
    writeInfo = getWriteInfo <$> memAddr <*> memOp <*> stall <*> branch
    -- interact with main memory (1 cycle delay)
    memData = mainRAM clk rst en (unpack <$> memAddr) writeInfo
    -- generate output
    writePair = getWritePair <$> writeReg <*> aluOut <*> memOpPrev <*> memData

    getWriteInfo addr (MemWrite' val) False Nothing = Just (unpack addr, val)
    getWriteInfo _ _ _ _ = Nothing

    getWritePair (Just no) _ MemLoad' res = Just (no, res)
    getWritePair (Just no) res _ _ = Just (no, res)
    getWritePair _ _ _ _ = Nothing

    check True _ = (Nothing, Nothing)
    check _ x = x
