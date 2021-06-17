module MIPS.InstrDef where

import MIPS.Prelude

-- The format of MIPS instructions
data Format
  = NoType -- type of nop
  | RType
      { op :: BitVector 6,
        rs :: BitVector 5,
        rt :: BitVector 5,
        rd :: BitVector 5,
        sa :: BitVector 5,
        fn :: BitVector 6
      }
  | IType
      { op :: BitVector 6,
        rs :: BitVector 5,
        rt :: BitVector 5,
        imm :: BitVector 16
      }
  | JType
      { op :: BitVector 6,
        lbl :: BitVector 26
      }

-- Transform raw instruction binary code into instruction format types.
decodeFormat :: BitVector 32 -> Format
decodeFormat 0 = NoType
decodeFormat 0xffffffff = NoType -- end instruction
decodeFormat vec =
  case slice d31 d26 vec of -- opcode
    0 -> toRType vec
    0x02 -> toJType vec
    0x03 -> toJType vec
    _ -> toIType vec
  where
    toRType =
      RType
        <$> slice d31 d26
        <*> slice d25 d21
        <*> slice d20 d16
        <*> slice d15 d11
        <*> slice d10 d6
        <*> slice d5 d0
    toIType =
      IType
        <$> slice d31 d26
        <*> slice d25 d21
        <*> slice d20 d16
        <*> slice d15 d0
    toJType = JType <$> slice d31 d26 <*> slice d25 d0

-- The inner form of MIPS instructions
data Instruction
  = NOP
  | LW RegNo RegNo (Signed 16)
  | SW RegNo RegNo (Signed 16)
  | ADD RegNo RegNo RegNo
  | ADDU RegNo RegNo RegNo
  | ADDI RegNo RegNo (Signed 16)
  | ADDIU RegNo RegNo (Unsigned 16)
  | SUB RegNo RegNo RegNo
  | SUBU RegNo RegNo RegNo
  | AND RegNo RegNo RegNo
  | ANDI RegNo RegNo (BitVector 16)
  | NOR RegNo RegNo RegNo
  | OR RegNo RegNo RegNo
  | ORI RegNo RegNo (BitVector 16)
  | XOR RegNo RegNo RegNo
  | XORI RegNo RegNo (BitVector 16)
  | SLL RegNo RegNo (Unsigned 5)
  | SRL RegNo RegNo (Unsigned 5)
  | SRA RegNo RegNo (Unsigned 5)
  | SLLV RegNo RegNo RegNo
  | SRLV RegNo RegNo RegNo
  | SRAV RegNo RegNo RegNo
  | BEQ RegNo RegNo (Signed 16)
  | BNE RegNo RegNo (Signed 16)
  | SLT RegNo RegNo RegNo
  | J (Unsigned 26)
  | JR (Unsigned 5)
  | JAL (Unsigned 26)
  deriving (Generic, NFDataX)

makeInstr instr a b c = instr (unpack a) (unpack b) (unpack c)

-- Transform instruction format types into instruction types.
decodeTyped :: Format -> Instruction
decodeTyped NoType = NOP
decodeTyped (RType 0 rs rt rd sa fn) =
  case fn of
    0x20 -> makeInstr ADD rs rt rd
    0x21 -> makeInstr ADDU rs rt rd
    0x24 -> makeInstr AND rs rt rd
    0x22 -> makeInstr SUB rs rt rd
    0x23 -> makeInstr SUBU rs rt rd
    0x27 -> makeInstr NOR rs rt rd
    0x25 -> makeInstr OR rs rt rd
    0x26 -> makeInstr XOR rs rt rd
    0x2a -> makeInstr SLT rs rt rd
    0x00 -> makeInstr SLL rd rt sa
    0x02 -> makeInstr SRL rd rt sa
    0x03 -> makeInstr SRA rd rt sa
    0x04 -> makeInstr SLLV rs rt rd
    0x06 -> makeInstr SRLV rs rt rd
    0x07 -> makeInstr SRAV rs rt rd
    0x08 -> JR (unpack rs)
decodeTyped (IType op rs rt imm) =
  case op of
    0x08 -> makeInstr ADDI rs rt imm
    0x09 -> makeInstr ADDIU rs rt imm
    0x0c -> makeInstr ANDI rs rt imm
    0x0d -> makeInstr ORI rs rt imm
    0x0e -> makeInstr XORI rs rt imm
    0x04 -> makeInstr BEQ rs rt imm
    0x05 -> makeInstr BNE rs rt imm
    0x23 -> makeInstr LW rs rt imm
    0x2b -> makeInstr SW rs rt imm
decodeTyped (JType op lbl) =
  case op of
    0x02 -> J (unpack lbl)
    0x03 -> JAL (unpack lbl)

-- Transform raw instruction binary code into instruction types.
decode :: BitVector 32 -> Instruction
decode vec = decodeTyped $ decodeFormat vec
