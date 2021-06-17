{-# OPTIONS -Wno-orphans #-}

module MIPS.Prelude
  ( module MIPS.Prelude,
    module Clash.Prelude,
  )
where

import Clash.Prelude
import GHC.Exts

instance IsString PortName where
  fromString = PortName

instance IsList PortName where
  type Item PortName = PortName
  fromList = PortProduct ""
  toList = error "toList for PortName is not implemented"

-- register number
type RegNo = Unsigned 5

-- register value
type RegVal = BitVector 32

-- PC value
type PCVal = Unsigned 32

-- Stalling state machine generator (unbundled).
stallMealyB ::
  (KnownDomain dom, NFDataX s, Bundle s) =>
  s -> -- initial state
  Clock dom ->
  Reset dom ->
  Enable dom ->
  (Signal dom Bool, Signal dom s) -> -- (stall signal, new state)
  Unbundled dom s -- output
stallMealyB init = exposeClockResetEnable $ mealyB stallT init
  where
    stallT old (True, new) = (init, init)
    stallT old (False, new) = (new, old)
