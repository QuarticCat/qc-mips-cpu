module MIPS.RegisterFile where

import MIPS.Prelude

{-# ANN
  registerFile
  ( Synthesize
      { t_name = "RegisterFile",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            PortProduct "RF" ["RS", "RT", "WRITE"]
          ],
        t_output =
          PortProduct "RF" ["RSV", "RTV"]
      }
  )
  #-}
-- Maintain the register values.
registerFile ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (RegNo, RegNo, Maybe (RegNo, RegVal)) -> -- (rs, rt, write pair)
  Signal System (RegVal, RegVal) -- (rs value, rt value)
registerFile = exposeClockResetEnable $ mealy registerFileT (replicate d32 0)
  where
    registerFileT regs (rs, rt, Nothing) = (regs, (regs !! rs, regs !! rt))
    registerFileT regs (rs, rt, Just (rd, rdv)) = (regs', (regs' !! rs, regs' !! rt))
      where
        regs' = replace rd rdv regs -- modified register values
