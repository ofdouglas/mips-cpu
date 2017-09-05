library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

-- TODO: Detect invalid opcodes!
--       Add decoding for a 'sys_opcode' class when implementing a TLB
--
-- * Coprocessor logic assumes we only use cp0, MUST be reviewed if adding an FPU.


entity instr_decoder is
  port (instruction       : in std_logic_vector(31 downto 0);

        -- Datapath muxes
        pc_jmp_br_mux     : out pc_jmp_br_mux_type;
        alu_in_B_mux      : out alu_in_B_mux_type;
        shamt_in_mux      : out shamt_in_mux_type;
        imm_ext_mux       : out imm_ext_mux_type;
        ex_out_mux        : out ex_out_mux_type;
        mem_out_mux       : out mem_out_mux_type;
        wb_out_mux        : out wb_out_mux_type;
        reg_dest_mux      : out reg_dest_mux_type;

        -- Datapath enables
        do_link           : out std_logic;
        do_load           : out std_logic;
        do_store          : out std_logic;
        do_jump_branch    : out std_logic;
        do_reg_write      : out std_logic;
        do_mdiv_op        : out std_logic;
        do_rs_read        : out std_logic;
        do_rt_read        : out std_logic;
        do_rfe            : out std_logic;
        do_cp0_wr         : out std_logic;
        do_cp0_rd         : out std_logic;
     -- do_sys_op         : out std_logic;
        
        -- sub-opcodes for functional units
        alu_opcode        : out std_logic_vector(3 downto 0);
        gen_opcode        : out std_logic_vector(3 downto 0);
        
        -- Exceptions passed down to CP0
        RI_exception      : out std_logic;      -- illegal instruction
        CpU_exception     : out std_logic;      -- bad coprocessor number
        SYS_exception     : out std_logic;      -- system call
        BRK_exception     : out std_logic       -- breakpoint
        );
end instr_decoder;


architecture arch of instr_decoder is

  signal instr_class : instruction_class_type;
  signal r_subclass  : r_subclass_type;
  signal i_subclass  : i_subclass_type;
  
  alias opcode    : std_logic_vector(5 downto 0) is instruction(31 downto 26);
  alias rs_num    : std_logic_vector(4 downto 0) is instruction(25 downto 21);
  alias rt_num    : std_logic_vector(4 downto 0) is instruction(20 downto 16);
  alias funct     : std_logic_vector(5 downto 0) is instruction(5 downto 0);
  alias r_special : std_logic_vector(2 downto 0) is instruction(2 downto 0);
  alias cp_num    : std_logic_vector(1 downto 0) is instruction(27 downto 26);

  -- subclass decoding (top 3 or 4 bits of significant field (opcode for R, etc))
  alias r_top4 : std_logic_vector(3 downto 0) is funct(5 downto 2);
  alias i_top3 : std_logic_vector(2 downto 0) is opcode(5 downto 3);

  -- Opcodes for functional units other than ALU
  -- Only one will be chosen to be 'gen_opcode'
  signal jump_opcode   : std_logic;
  signal branch_opcode : std_logic_vector(3 downto 0);
  signal shift_opcode  : std_logic_vector(3 downto 0);
  signal dmem_opcode   : std_logic_vector(3 downto 0);
  signal mdiv_opcode   : std_logic_vector(3 downto 0);

-- Will probably be used for TLB instructions
-- signal sys_opcode    : std_logic_vector(3 downto 0);

  
  -- flags used internally 
  signal branch_flag   : std_logic;
  signal jump_flag     : std_logic;     
  signal link_flag     : std_logic;     -- jal, bgtzal, etc
  signal mem_flag      : std_logic;     -- lw, sw, etc
  signal shift_flag    : std_logic;
  signal mdiv_flag     : std_logic;
  signal lo_hi_flag    : std_logic;
  signal lui_flag      : std_logic;
  signal from_cp_flag  : std_logic;     -- coproc read: MFCz, CFCz
  signal to_cp_flag    : std_logic;     -- coproc write: MTCz, CTCz
--  signal sys_flag      : std_logic;

begin

  -- Top level decode
  ----------------------------------------------------------------------
  instr_class <= R_TYPE      when opcode = "000000" else
                 REGIMM_TYPE when opcode = "000001" else
                 COPROC_TYPE when std_match(opcode, "0100--") else
                 J_TYPE      when std_match(opcode, "0000--") else
                 I_TYPE;

  
  -- Subclass decode
  ----------------------------------------------------------------------
  r_subclass <= R_TYPE_SET      when std_match(r_top4, R_TYPE_TOP4_SET)   else
                R_TYPE_ALU      when std_match(r_top4, R_TYPE_TOP4_ALU)   else
                R_TYPE_MDIV     when std_match(r_top4, R_TYPE_TOP4_MDIV)  else
                R_TYPE_LO_HI    when std_match(r_top4, R_TYPE_TOP4_LO_HI) else
                R_TYPE_SYS      when std_match(r_top4, R_TYPE_TOP4_SYS)   else
                R_TYPE_JUMP     when std_match(r_top4, R_TYPE_TOP4_JUMP)  else
                R_TYPE_SHIFT; --when std_match(r_top4, R_TYPE_TOP3_SHIFT)
  
  with i_top3 select i_subclass <=
    I_TYPE_STORE       when I_TYPE_TOP3_STORE,
    I_TYPE_LOAD        when I_TYPE_TOP3_LOAD,
    I_TYPE_ALU         when I_TYPE_TOP3_ALU,
    I_TYPE_BRANCH      when others;  -- I_TYPE_TOP3_BRANCH;


  -- Decoding flags (internal use only, mostly in gen_opcode mux)
  ----------------------------------------------------------------------

  branch_flag <=
    '1' when instr_class = I_TYPE and i_subclass = I_TYPE_BRANCH else
    '1' when instr_class = REGIMM_TYPE else
    '0';

  jump_flag <=
    '1' when instr_class = R_TYPE and r_subclass = R_TYPE_JUMP else
    '1' when instr_class = J_TYPE else
    '0';

  link_flag <=
    '1' when instr_class = REGIMM_TYPE and instruction(REGIMM_LINK_BIT) = '1' else
    '1' when instr_class = J_TYPE and instruction(J_TYPE_LINK_BIT) = '1' else
    '1' when instr_class = R_TYPE and r_subclass = R_TYPE_JUMP and
             instruction(R_TYPE_LINK_BIT) = '1' else
    '0';

  shift_flag <=
    '1' when instr_class = R_TYPE and r_subclass = R_TYPE_SHIFT else
    '0';
  
  mem_flag <= 
    '1' when instr_class = I_TYPE and i_subclass = I_TYPE_LOAD else
    '1' when instr_class = I_TYPE and i_subclass = I_TYPE_STORE else
    '0';

  mdiv_flag <=
    '1' when instr_class = R_TYPE and r_subclass = R_TYPE_MDIV else
    '0';

  lo_hi_flag <=
    '1' when instr_class = R_TYPE and r_subclass = R_TYPE_LO_HI else
    '0';
  
  lui_flag <=
    '1' when instr_class = I_TYPE and i_subclass = I_TYPE_ALU and
             opcode(2 downto 0) = "111" else
    '0';

  from_cp_flag <= '1' when instr_class = COPROC_TYPE and
                         instruction(COPROC_MOVE_TO_BIT) = '0' else
                  '0';
  
  to_cp_flag <= '1' when instr_class = COPROC_TYPE and
                         instruction(COPROC_MOVE_TO_BIT) = '1' else
                '0';

  -- Not used yet:
  --sys_flag <=
  --  '1' when instr_class = R_TYPE and r_subclass = R_TYPE_SYS else
  --  '1' when instr_class = COPROC_TYPE else
  --  '0';

                
  -- Functional Unit Opcodes: Don't extend to full length until before
  -- the gen_opcode mux, because some are used elswhere in decoding.
  ----------------------------------------------------------------------

  jump_opcode <= J_CODE when instr_class = J_TYPE else JR_CODE;

  shift_opcode <= "00" & funct(1 downto 0);

  -- MDIV and LO_HI opcodes are the same low2 bits; set MSB for MDIV to decode
  mdiv_opcode <=
    "10" & funct(1 downto 0) when mdiv_flag = '1' else
    "00" & funct(1 downto 0);

  branch_opcode <=
    '0' & opcode(2 downto 0)      when instr_class = I_TYPE else
    "00" & rt_num(1 downto 0); -- when instr_class = REGIMM_TYPE

  -- Load/Store: extract 'unsigned' bit from LBU/LHU and make it dmem_opcode(3)
  dmem_opcode <= "10" & opcode(1 downto 0) when opcode(2 downto 1) = "10" else
                 '0' & opcode(2 downto 0);

  alu_opcode_proc:
  process (instruction, instr_class, i_subclass, mem_flag, link_flag)
  begin
    alu_opcode <= funct(3 downto 0);    -- default for R_TYPE

    if mem_flag = '1' or link_flag = '1' then
      alu_opcode <= ADDU_CODE;
    end if;

    -- I-TYPE ALU and SLT stuff is mixed together
    if instr_class = I_TYPE and i_subclass = I_TYPE_ALU then
      if opcode(3 downto 1) = "101" then -- SLTI/SLTIU
        alu_opcode <= '1' & opcode(2 downto 0);
      else
        alu_opcode <= '0' & opcode(2 downto 0);
      end if;
    end if;
  end process;

  
  gen_opcode <=
    dmem_opcode         when mem_flag = '1' else
    branch_opcode       when branch_flag = '1' else
    shift_opcode        when shift_flag = '1' else    
    mdiv_opcode         when mdiv_flag = '1' else
    mdiv_opcode         when lo_hi_flag = '1' else
 -- sys_opcode          when sys_flag = '1' else
    "000" & jump_opcode;
  
    
  -- Datapath Enables
  ----------------------------------------------------------------------


  do_link <= link_flag;

  do_load <=
    '1' when mem_flag = '1' and i_subclass = I_TYPE_LOAD else
    '0';
  
  do_store <=
    '1' when mem_flag = '1' and i_subclass = I_TYPE_STORE else
    '0';

  SYS_exception <=
    '1' when instr_class = R_TYPE and r_subclass = R_TYPE_SYS and
             instruction(R_TYPE_SYS_BIT) = R_SYS_SYSCALL_CODE else
    '0';

  BRK_exception <=
    '1' when instr_class = R_TYPE and r_subclass = R_TYPE_SYS and
             instruction(R_TYPE_SYS_BIT) = R_SYS_BREAK_CODE else
    '0';

  do_rfe <=
    '1' when instr_class = COPROC_TYPE and instruction(COPROC_RFE_BIT) = '1' else
    '0';

  --
  -- CHANGE THIS IF OTHER COPROCESSORS ARE ADDED!
  --
  do_cp0_wr <=
    '1' when instr_class = COPROC_TYPE and to_cp_flag = '1' else
    '0';
  
  -- used in setting BD bit on exception
  do_jump_branch <= jump_flag or branch_flag;
  
  -- Hardcoded for now because we don't have an FPU or other coproc
  CpU_exception <= '1' when instr_class = COPROC_TYPE and cp_num /= "00" else '0';

  -- Hardcoded for now because this decoding style makes it tough to
  -- reliably detect invalid opcodes! May need to go to ROM-style decoder
  -- to do that...
  RI_exception <= '0';
  
  --do_sys_op <=
  --  '1' when instr_class = COPROC_TYPE else
  --  '1' when instr_class = R_TYPE and r_subclass = R_TYPE_SYS else
  --  '0';

  do_cp0_rd <= from_cp_flag;
  
  do_mdiv_op <= mdiv_flag or lo_hi_flag;

  
  do_reg_write_proc:
  process (instruction, instr_class, r_subclass, i_subclass, mdiv_opcode, from_cp_flag)
  begin
    -- Default
    do_reg_write <= '1';

    if instr_class = R_TYPE then
      if r_subclass = R_TYPE_MDIV and
        (funct(1 downto 0) /= MFLO_CODE and funct(1 downto 0) /= MFHI_CODE) then
        do_reg_write <= '0';
      end if;

      if r_subclass = R_TYPE_JUMP and instruction(R_TYPE_LINK_BIT) = '0' then
        do_reg_write <= '0';
      end if;
      
      if r_subclass = R_TYPE_SYS then
        do_reg_write <= '0';
      end if;
    end if;

    if instr_class = I_TYPE and
      (i_subclass = I_TYPE_STORE or i_subclass = I_TYPE_BRANCH) then
      do_reg_write <= '0';
    end if;

    if instr_class = COPROC_TYPE then
      if from_cp_flag = '1' then
        do_reg_write <= '1';
      end if;
    end if;
    
    if instr_class = J_TYPE and instruction(J_TYPE_LINK_BIT) = '0' then
      do_reg_write <= '0';
    end if;
    
    if instr_class = REGIMM_TYPE then
      do_reg_write <= '0';
    end if;
  end process;
  

  do_rs_rt_read_proc:
  process (instruction, instr_class, r_subclass, i_subclass, mdiv_opcode,
           lui_flag, to_cp_flag)
  begin
    do_rs_read <= '0';
    do_rt_read <= '0';

    case instr_class is
      when R_TYPE =>
        if r_subclass = R_TYPE_SET or
           r_subclass = R_TYPE_ALU or
           r_subclass = R_TYPE_MDIV then
          do_rs_read <= '1';
          do_rt_read <= '1';
        end if;

        if r_subclass = R_TYPE_JUMP then
          do_rs_read <= '1';
        end if;

        if r_subclass = R_TYPE_SHIFT then
          do_rt_read <= '1';
          if instruction(SHIFT_VARIABLE_BIT) = '0' then
            do_rs_read <= '1';
          end if;
        end if;

        -- MTLO/MTHI
        if r_subclass = R_TYPE_LO_HI and mdiv_opcode(0) = '1' then
          do_rs_read <= '1';
        end if;
        
      when I_TYPE =>
        if i_subclass = I_TYPE_LOAD then
          do_rs_read <= '1';
        end if;

        if i_subclass = I_TYPE_STORE then
          do_rs_read <= '1';
          do_rt_read <= '1';
        end if;
        
        if i_subclass = I_TYPE_BRANCH then
          do_rs_read <= '1';
          do_rt_read <= '1';
        end if;

        if i_subclass = I_TYPE_ALU and lui_flag = '0' then
          do_rs_read <= '1';
        end if;
        

      when REGIMM_TYPE =>
        do_rs_read <= '1';

        
      when COPROC_TYPE =>
        if to_cp_flag = '1' then
          do_rs_read <= '1';
        end if;

        
      when J_TYPE =>
        -- J_TYPE doesn't read registers
                                        
    end case;
  end process;
    
  
  -- Datapath Muxes
  ----------------------------------------------------------------------

  pc_jmp_br_mux <=
    DO_BRANCH    when branch_flag = '1' else
    DO_JUMP;  -- when jump_flag = '1' else

  reg_dest_mux <=
    USE_RT_REG    when instr_class = COPROC_TYPE or instr_class = I_TYPE else
    USE_LINK_REG  when link_flag = '1' else
    USE_RD_REG;

  alu_in_B_mux <=
    USE_IMMEDIATE when instr_class = I_TYPE or (link_flag or mem_flag) = '1' else
    USE_RT_DATA;

  -- Only zero extend for ANDI, ORI instructions
  imm_ext_mux <=
    DO_ZERO_EXTEND when instr_class = I_TYPE and i_subclass = I_TYPE_ALU and
                        instruction(I_TYPE_0_EXTEND_BIT) = '1' else
    DO_SIGN_EXTEND;

  shamt_in_mux <=
    USE_RS_DATA       when instruction(SHIFT_VARIABLE_BIT) = '1' else
    USE_SHAMT_FIELD;

  ex_out_mux <=
    USE_SHIFT_RESULT when shift_flag = '1' else
    USE_MDIV_RESULT  when lo_hi_flag = '1' else
    USE_LUI_RESULT   when lui_flag = '1'   else
    USE_ALU_RESULT;

  mem_out_mux <=
    USE_DMEM_RESULT when mem_flag = '1' else
    USE_CP0_RESULT;

  wb_out_mux <=
    USE_MEM_RESULT  when mem_flag = '1' else
    USE_MEM_RESULT  when from_cp_flag = '1' else
    USE_EX_RESULT;

   
end arch;
