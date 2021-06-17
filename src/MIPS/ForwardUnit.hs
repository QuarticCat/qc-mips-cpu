module MIPS.ForwardUnit where

import MIPS.Prelude

type ForwardInfo = Maybe (RegNo, RegVal)

{-# ANN
  forwardUnit
  ( Synthesize
      { t_name = "ForwardUnit",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            "FORWARD_A",
            "FORWARD_B",
            "RS",
            "RT"
          ],
        t_output =
          PortProduct "FW" ["OVERRIDE_RS", "OVERRIDE_RT"]
      }
  )
  #-}
-- Handle data hazards.
-- Check if rs or rt should be overridden.
forwardUnit ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System ForwardInfo -> -- exec register, from memory stage
  Signal System ForwardInfo -> -- load register, from write-back stage
  Signal System RegNo -> -- rs
  Signal System RegNo -> -- rt
  Signal System (Maybe (BitVector 32), Maybe (BitVector 32)) -- (overridden rs value, overridden rt value)
forwardUnit = exposeClockResetEnable forwardUnit'
  where
    forwardUnit' a b c d = forwarding <$> a <*> b <*> c <*> d
    forwarding exec load rs rt = (rsv, rtv)
      where
        rsv =
          case (exec, load) of
            (Just (no, res), _) | no == rs -> Just res
            (_, Just (no, res)) | no == rs -> Just res
            _ -> Nothing
        rtv =
          case (exec, load) of
            (Just (no, res), _) | no == rt -> Just res
            (_, Just (no, res)) | no == rt -> Just res
            _ -> Nothing
