module MIPS.InstrModule where

import MIPS.InstrDef
import MIPS.Prelude
import MIPS.RAM

type BranchVal = Maybe (Unsigned 32)

-- A state machine that maintains the value of PC.
programCounter ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System BranchVal -> -- branch value
  Signal System PCVal -- PC value
programCounter = exposeClockResetEnable $ mealy programCounterT 0
  where
    -- if no branch, increase PC
    programCounterT pc Nothing = (pc + 1, pc)
    -- if there is branch, substituted with branch value
    programCounterT pc (Just branch) = (branch + 1, branch)

{-# ANN
  instrModule
  ( Synthesize
      { t_name = "InstrModule",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            "STALL",
            "BRANCH"
          ],
        t_output =
          PortProduct "PC" ["INSTRUCTION", "VALUE"]
      }
  )
  #-}
-- Fetch instruction and transform it to inner form.
instrModule ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool -> -- stall signal
  Signal System BranchVal -> -- branch value
  Signal System (Instruction, PCVal) -- (instruction, PC value)
instrModule clk rst en stall branch = bundle (instr, pc)
  where
    -- get PC
    pc = programCounter clk rst en branch
    -- transform instruction to inner form
    -- if stall, then return NOP
    getInstr s r = if s then NOP else decode r
    -- fetch instruction from `instrRAM` and transform
    instr = getInstr <$> stall <*> instrRAM clk rst en pc
