module MIPS.HazardUnit where

import MIPS.ArithModule
import MIPS.ControlUnit
import MIPS.Prelude

type HazardInput = Maybe (Unsigned 32) -- branching target

{-# ANN
  hazardUnit
  ( Synthesize
      { t_name = "HazardUnit",
        t_inputs = ["BRANCH"],
        t_output = "STALL"
      }
  )
  #-}
-- Only handle control hazards. Data hazards are handled by Forward Unit.
-- Send stall signal when there is a branching action.
hazardUnit :: HazardInput -> Bool
hazardUnit (Just _) = True
hazardUnit _ = False
