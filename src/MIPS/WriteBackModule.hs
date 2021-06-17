module MIPS.WriteBackModule where

import MIPS.MemModule
import MIPS.Prelude
import MIPS.RAM

{-# ANN
  writeBackModule
  ( Synthesize
      { t_name = "WriteBackModule",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            "BRANCH",
            "WRITE_PAIR"
          ],
        t_output =
          PortProduct "WB" ["BRANCH", "WRITE_PAIR"]
      }
  )
  #-}
-- Simply a buffer before editing the data.
writeBackModule ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Maybe (Unsigned 32)) -> -- branch
  Signal System (Maybe (RegNo, RegVal)) -> -- write pair
  Signal System (Maybe (Unsigned 32), Maybe (RegNo, RegVal)) -- (branch, write pair)
writeBackModule clk rs en branch writePair = writeReg $ bundle (branch, writePair)
  where
    -- state register
    writeReg = exposeClockResetEnable (register (Nothing, Nothing)) clk rs en
