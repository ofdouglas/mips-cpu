--
-- Opcodes and control codes
--

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;

-- Destination register is ALWAYS either RD or RT.
-- RS is primary source register
-- RT is secondary source register


package MIPS_r3000_ISA is

----------------------------------------------------------------------
-- Top-level instruction classes (based on OPCODE field)
----------------------------------------------------------------------
-- CLASS     OPCODE  EXTRA_FIELD    NOTES
----------------------------------------------------------------------
-- R-TYPE:   000000  FUNCT  (5:0)   sometimes called 'special' 
-- REGIMM:   000001  RT    (20:16)
-- COPROC:   0100--  FUNCT  (5:0)   also uses RS (25:21) for MTC, MFC
-- J-TYPE:   0000--  none           
-- I-TYPE:   ------  none

  
  type INSTRUCTION_CLASS_TYPE is
    (R_TYPE, I_TYPE, J_TYPE, REGIMM_TYPE, COPROC_TYPE);

  
----------------------------------------------------------------------
-- R-TYPE subclasses (based on top 4 bits of FUNCT field)
----------------------------------------------------------------------
-- top4       class
----------------------------------------------------------------------
-- 101-       set 
-- 100-       add/sub/bool
-- 011-       mult/div
-- 010-       move from/to
-- 0011       system (break/syscall)
-- 0010       jump
-- 000-       shift
--

  type R_SUBCLASS_TYPE is (R_TYPE_SET, R_TYPE_ALU, R_TYPE_MDIV,
                           R_TYPE_LO_HI, R_TYPE_SYS,
                           R_TYPE_JUMP, R_TYPE_SHIFT);
  
  constant R_TYPE_TOP4_SET     : std_logic_vector(3 downto 0) := "101-";
  constant R_TYPE_TOP4_ALU     : std_logic_vector(3 downto 0) := "100-";
  constant R_TYPE_TOP4_MDIV    : std_logic_vector(3 downto 0) := "011-";
  constant R_TYPE_TOP4_LO_HI   : std_logic_vector(3 downto 0) := "010-";
  constant R_TYPE_TOP4_SYS     : std_logic_vector(3 downto 0) := "0011";
  constant R_TYPE_TOP4_JUMP    : std_logic_vector(3 downto 0) := "0010";
  constant R_TYPE_TOP4_SHIFT   : std_logic_vector(3 downto 0) := "000-";

  
----------------------------------------------------------------------  
-- I-TYPE subclasses (based on top 3 bits of OPCODE field)
----------------------------------------------------------------------
-- top3       class
----------------------------------------------------------------------
-- 101        store
-- 100        load
-- 001        add/bool/lui/set
-- 000        branch

  type I_SUBCLASS_TYPE is (I_TYPE_STORE, I_TYPE_LOAD, I_TYPE_ALU, I_TYPE_BRANCH);
  
  constant I_TYPE_TOP3_STORE   : std_logic_vector(2 downto 0) := "101";
  constant I_TYPE_TOP3_LOAD    : std_logic_vector(2 downto 0) := "100";
  constant I_TYPE_TOP3_ALU     : std_logic_vector(2 downto 0) := "001";
  constant I_TYPE_TOP3_BRANCH  : std_logic_vector(2 downto 0) := "000";


-- Index of various flags in the instruction (for instruction decoder only)
  constant J_TYPE_LINK_BIT     : integer := 26; -- 
  constant REGIMM_LINK_BIT     : integer := 20; -- 
  constant SHIFT_VARIABLE_BIT  : integer := 2;  --
  constant COPROC_MOVE_TO_BIT  : integer := 23; -- 
  constant COPROC_CO_BIT       : integer := 25; -- Not sure what this is for
  constant COPROC_RFE_BIT      : integer := 4;  -- Only set for RFE instruction
  constant I_TYPE_0_EXTEND_BIT : integer   := 28;
  constant R_TYPE_SYS_BIT      : integer   := 0;
  constant R_TYPE_LINK_BIT     : integer   := 0;
  constant R_SYS_BREAK_CODE    : std_logic := '1';
  constant R_SYS_SYSCALL_CODE  : std_logic := '0';

  
----------------------------------------------------------------------
-- DECODINGS -- Inputs to functional units.
----------------------------------------------------------------------  
-- The ALU has it's own opcode because it is used by many different
-- instructions (e.g. LOAD, JAL, etc).
--
-- All other functional units share the 4-bit 'gen_opcode' field. Some
-- functional units extract a few bits of 'gen_opcode' and ignore the
-- rest, or treat some of the remaining bits as flags.
--  
-- A 'do_xxxx' enable signal controls units that can modify registers
-- (e.g. LOAD, JUMP, CP0, etc). All other units' results can be safely
-- ignored when not needed (e.g SHIFT, etc) - so it is safe to pass
-- them a 'gen_opcode' meant for a different unit.
----------------------------------------------------------------------

  -- ALU Codes
  constant SLT_CODE           : std_logic_vector(3 downto 0) := "1010";
  constant SLTU_CODE          : std_logic_vector(3 downto 0) := "1011";
  constant NOR_CODE           : std_logic_vector(3 downto 0) := "0111";
  constant XOR_CODE           : std_logic_vector(3 downto 0) := "0110"; 
  constant OR_CODE            : std_logic_vector(3 downto 0) := "0101";
  constant AND_CODE           : std_logic_vector(3 downto 0) := "0100";
  constant SUBU_CODE          : std_logic_vector(3 downto 0) := "0011";    
  constant SUB_CODE           : std_logic_vector(3 downto 0) := "0010";
  constant ADDU_CODE          : std_logic_vector(3 downto 0) := "0001";
  constant ADD_CODE           : std_logic_vector(3 downto 0) := "0000";

  -- SHIFT Codes
  constant SLL_CODE           : std_logic_vector(1 downto 0) := "00";
  constant SRL_CODE           : std_logic_vector(1 downto 0) := "10";
  constant SRA_CODE           : std_logic_vector(1 downto 0) := "11";
  
  -- BRANCH Codes
  constant BGTZ_CODE          : std_logic_vector(2 downto 0) := "111";
  constant BLEZ_CODE          : std_logic_vector(2 downto 0) := "110";
  constant BNE_CODE           : std_logic_vector(2 downto 0) := "101";  
  constant BEQ_CODE           : std_logic_vector(2 downto 0) := "100";
  constant BGEZ_CODE          : std_logic_vector(2 downto 0) := "001";
  constant BLTZ_CODE          : std_logic_vector(2 downto 0) := "000";

  -- JUMP Codes
  constant JR_CODE            : std_logic := '1';
  constant J_CODE             : std_logic := '0';
  
  -- DMEM Codes
  constant LS_UNSIGNED_BITNUM : integer := 3;
  constant LS_BYTE_CODE       : std_logic_vector := "000";
  constant LS_HALF_CODE       : std_logic_vector := "001";
  constant LS_WORD_CODE       : std_logic_vector := "011";
  constant LS_LEFT_CODE       : std_logic_vector := "010";
  constant LS_RIGHT_CODE      : std_logic_vector := "110";
  
  -- MDIV Codes
  constant DIVU_CODE  : std_logic_vector(1 downto 0) := "11";  
  constant DIV_CODE   : std_logic_vector(1 downto 0) := "10";
  constant MULTU_CODE : std_logic_vector(1 downto 0) := "01";  
  constant MULT_CODE  : std_logic_vector(1 downto 0) := "00";
  constant MTLO_CODE  : std_logic_vector(1 downto 0) := "11";
  constant MFLO_CODE  : std_logic_vector(1 downto 0) := "10";
  constant MTHI_CODE  : std_logic_vector(1 downto 0) := "01";
  constant MFHI_CODE  : std_logic_vector(1 downto 0) := "00";
  
  constant LUI_OPCODE     : std_logic_vector(5 downto 0) := "001111";

 
  
-- Datapath Multiplexor control signal types
----------------------------------------------------------------------
  type pc_jmp_br_mux_type is (DO_JUMP, DO_BRANCH);

  type do_link_mux_type is (DO_LINK, DO_NOT_LINK);
  
  type alu_in_B_mux_type is (USE_RT_DATA, USE_IMMEDIATE);

  type shamt_in_mux_type is (USE_SHAMT_FIELD, USE_RS_DATA);
  
  type ex_out_mux_type is (USE_ALU_RESULT, USE_SHIFT_RESULT,
                           USE_MDIV_RESULT, USE_LUI_RESULT);
  
  type imm_ext_mux_type is (DO_SIGN_EXTEND, DO_ZERO_EXTEND);
  
  -- For JAL, et. al.
  type rd_num_mux_type is (USE_RD_NUM, USE_LINK_REG);
  
  type mem_out_mux_type is (USE_DMEM_RESULT, USE_CP0_RESULT);
  
  type wb_out_mux_type is (USE_MEM_RESULT, USE_EX_RESULT);
  
  type reg_dest_mux_type is (USE_RD_REG, USE_RT_REG, USE_LINK_REG);

  type rs_fwd_mux_type is (FWD_RS_FROM_EX, FWD_RS_FROM_MEM, RS_LOCAL);

  type rt_fwd_mux_type is (FWD_RT_FROM_EX, FWD_RT_FROM_MEM, RT_LOCAL);
  
  type mem_rt_fwd_mux_type is (FWD_RT_FROM_MEM, RT_LOCAL);
  
  type exc_vector_mux_type is (EXC_GENERAL, EXC_TLB_MISS, EXC_RESET);
  
  
-- MEMORY CONSTANTS
----------------------------------------------------------------------

-- Virtual addresses for exception handlers
  constant EXC_TLB_MISS_VADDR         : unsigned(31 downto 0) := X"8000_0000";
  constant EXC_GENERAL_VADDR          : unsigned(31 downto 0) := X"8000_0080";

  -- Values we are currently using, until the linker script is fixed:
  constant EXC_TLB_MISS_NOCACHE_VADDR : unsigned(31 downto 0) := X"bfc0_0008";
  constant EXC_GENERAL_NOCACHE_VADDR  : unsigned(31 downto 0) := X"bfc0_0010";
  constant EXC_RESET_VADDR            : unsigned(31 downto 0) := X"bfc0_0000";

  -- The actual values specified by the MIPS1 ISA:
  --constant EXC_TLB_MISS_NOCACHE_VADDR : unsigned(31 downto 0) := X"bfc0_0100";
  --constant EXC_GENERAL_NOCACHE_VADDR  : unsigned(31 downto 0) := X"bfc0_0180";
  --constant EXC_RESET_VADDR            : unsigned(31 downto 0) := X"bfc0_0000";
  
-- Physical addresses for exception handlers:
  constant TLB_MISS_KUSEG_PADDR         : unsigned(31 downto 0) := X"0000_0000";
  constant GEN_EXCEPTION_PADDR          : unsigned(31 downto 0) := X"0000_0080";
  constant TLB_MISS_KUSEG_NOCACHE_PADDR : unsigned(31 downto 0) := X"1fc0_0100";
  constant GEN_EXCEPTION_NOCACHE_PADDR  : unsigned(31 downto 0) := X"1fc0_0180";
  constant RESET_EXCEPTION_PADDR        : unsigned(31 downto 0) := X"1fc0_0000";

  -- RESET vector would also be used for other types of NMI

  
-- EXCEPTION CODES
----------------------------------------------------------------------

  constant Int_ExcCode   : unsigned(4 downto 0) := "00000";
  constant Mod_ExcCode   : unsigned(4 downto 0) := "00001";
  constant TLBL_ExcCode  : unsigned(4 downto 0) := "00010";
  constant TLBS_ExcCode  : unsigned(4 downto 0) := "00011";
  constant AdEL_ExcCode  : unsigned(4 downto 0) := "00100";
  constant AdES_ExcCode  : unsigned(4 downto 0) := "00101";
  constant IBE_ExcCode   : unsigned(4 downto 0) := "00110";
  constant DBE_ExcCode   : unsigned(4 downto 0) := "00111";
  constant SYS_ExcCode   : unsigned(4 downto 0) := "01000";
  constant BRK_ExcCode   : unsigned(4 downto 0) := "01001";
  constant RI_ExcCode    : unsigned(4 downto 0) := "01010";
  constant CpU_ExcCode   : unsigned(4 downto 0) := "01011";
  constant Ov_ExcCode    : unsigned(4 downto 0) := "01101";
  -- 13-31 are reserved

----------------------------------------------------------------------
--  1 - Int     : Interrupt from external device or software
--  2 - Mod     : "TLB Modified"
--  3 - TLBL    : TLB Miss on Load
--  4 - TLBS    : TLB Miss on Store
--  5 - AdEL    : Address error on load / instruction fetch. Can be an 
--                alignment error, or a user access outside of kuseg.
--  6 - AdES    : Address error on store. Same conditions as AdEL.
--  7 - IBE     : Instruction bus error.
--  8 - DBE     : Data bus error. Never asserted for writes, because a 
--                write buffer would makes it imprecise.
--  9 - SYS     : System call, by the 'syscall' instruction.
-- 10 - BRK     : Breakpoint, by the 'break' instruction.
-- 11 - RI      : Reserved instruction (illegal opcode).
-- 12 - CpU     : Coprocessor Unusable.
-- 13 - Ov      : Arithmetic overflow.

  
-- CP0 Register Numbers
----------------------------------------------------------------------


  constant EPC_REG_NUM      : unsigned(4 downto 0) := "01110";
  constant CAUSE_REG_NUM    : unsigned(4 downto 0) := "01101";
  constant STATUS_REG_NUM   : unsigned(4 downto 0) := "01100";
  constant COMPARE_REG_NUM  : unsigned(4 downto 0) := "01011";
  constant COUNT_REG_NUM    : unsigned(4 downto 0) := "01001";
  constant BADVADDR_REG_NUM : unsigned(4 downto 0) := "01000";


  
  constant WORD_SIZE : positive := 32;
  constant WORD_LOG2 : positive := positive(ceil(log2(real(WORD_SIZE))));
    
  
end package MIPS_r3000_ISA;


package body MIPS_r3000_ISA is
end MIPS_r3000_ISA;


  
