module MIPS.RAM where

import MIPS.Prelude

-- memory address
type MemAddr = Unsigned 32

-- memory block
type MemBlock = BitVector 32

-- Instruction RAM. Read only. Read `instructions.bin` as initial value.
instrRAM ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System MemAddr -> -- fetch address
  Signal System MemBlock -- read data
instrRAM = exposeClockResetEnable instrRAM'
  where
    -- only accept reading, fill write pair with `pure Nothing`
    instrRAM' addr = blockRamFile d512 "instructions.bin" addr (pure Nothing)

{-# ANN
  mainRAM
  ( Synthesize
      { t_name = "MainMemory",
        t_inputs =
          [ "CLOCK",
            "RESET",
            "ENABLE",
            "FETCH_ADDRESS",
            "EDIT_SERIAL"
          ],
        t_output = "DATA"
      }
  )
  #-}
-- Main memory. 512 blocks. Initially filled with zeros.
mainRAM ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System MemAddr -> -- fetch address
  Signal System (Maybe (MemAddr, MemBlock)) -> -- write pair
  Signal System MemBlock -- read data
mainRAM = exposeClockResetEnable $ blockRam (replicate d512 0)
