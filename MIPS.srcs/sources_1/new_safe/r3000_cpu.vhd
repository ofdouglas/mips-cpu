library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.MIPS_r3000_ISA.ALL;


-- MIPS r3000 processor
----------------------------------------------------------------------------  
-- Version 0: Full integer instruction set is supported, but without caches,
-- TLB-MMU, or FPU.


-- RESET / INITIALIZATION
----------------------------------------------------------------------------
-- The top-level entity will assert reset for at least four CPU clocks during
-- startup. When 'reset' or the 'exception_flush' signals are active, all
-- control signals are zeroed in every pipeline stage. Additionally, the PC is
-- forced to the EXC_RESET_VADDR exception vector, and NOPs are loaded into the
-- pipeline at the IF_ID_instruction register. All data registers remain enabled,
-- so that after four clock cycles, every pipeline stage contains a NOP.


-- Deviations from MIPS1 Architecture
----------------------------------------------------------------------------
-- The exception vectors are not located at the traditional addresses, see
-- MIPS_r3000_ISA for more info.
--
-- Virtual to physical address mapping is not implemented yet. Even a MIPS
-- CPU without an MMU is supposed to translate the upper few bits of an
-- address, but that is not implemented yet.


-- Non-critical Issues:
----------------------------------------------------------------------------
-- A NOP such as 'addu $0 $0 $0' can cause a stall because 'do_reg_write' gets set.
-- The program still runs correctly but the stall is unneeded and unexpected.
-- Either fix the hazard unit to to check for reg_dest_num = 0 or modify the
-- instruction decoder to not set 'do_reg_write' when the destination is 0.
-- IF the decoder is changed, revise the regfile! Check the jal test program.
-- 
-- The RFE instruction seems to cause brief but unecessarry stalls.
--


-- TODO - LATER
----------------------------------------------------------------------------
-- MEM_WB gets EX_out as well as MEM_out, mux in WB decides which one to write
-- back to regfile. Consider moving that mux to end of MEM stage.
--
-- Consider if MDIV location is appropriate for exception semantics
--
-- Speed up divider
--
-- review jump_branch_stall_proc (hazard unit) for inefficiencies
--






entity r3000_cpu is
  port (
    -- control signals
    clk            : in std_logic;
    reset          : in std_logic;
    
    -- hardware interrupt line 0 is tied to the counter compare match interrupt
    -- inside of coprocessor 0. It should be removed from here eventually.
    hw_ext_interrupts  : in std_logic_vector(5 downto 0);
    exception_out      : out std_logic;

    -- instruction bus interface
    ibus_addr      : out unsigned(31 downto 0);
    ibus_rd_data   : in std_logic_vector(31 downto 0);
    ibus_rd_en     : out std_logic;
    ibus_fault     : in std_logic;
    
    -- Data Bus Control Port
    dbus_lane_en   : out std_logic_vector(0 to 3);
    dbus_rd_en     : out std_logic;
    dbus_wr_en     : out std_logic;
    
    -- Data Bus Data Port
    dbus_addr      : out unsigned(31 downto 0);
    dbus_rd_data   : in std_logic_vector(31 downto 0);
    dbus_wr_data   : out std_logic_vector(31 downto 0);
    dbus_fault     : in std_logic
    );
  
end r3000_cpu;

architecture arch of r3000_cpu is

  -- PIPELINE STAGES : All pipeline registers are written in AA_BB_... format,
  -- where AA and BB are (all-caps) shorthand for two different pipeline stages.
  -- Signals in the format AA_... are local to that stage, and *not* registered.
  --
  -- Some signals get initial values here, but they probably don't need it, since
  -- reset will assign them the same values.

  
  ----------------------------------------------------------------------------  
  -- Instruction Fetch Stage
  ----------------------------------------------------------------------------  
  -- Exceptions involving the PC are generated in the ID stage for performance
  -- reasons.
  
  -- Program counter
  signal IF_pc_current      : unsigned(31 downto 0);
  signal IF_pc_plus_4       : unsigned(31 downto 0);
  signal IF_pc_next         : unsigned(31 downto 0);
  signal IF_pc_jmp_br_mux   : pc_jmp_br_mux_type;

  signal IF_instruction     : std_logic_vector(31 downto 0);
  
  ----------------------------------------------------------------------------  
  -- Instruction Fetch / Decode Pipeline Registers
  ----------------------------------------------------------------------------
  signal IF_ID_instruction  : std_logic_vector(31 downto 0) := (others => '0');
  signal IF_ID_pc_current   : unsigned(31 downto 0) := EXC_RESET_VADDR;
  signal IF_ID_pc_plus_4    : unsigned(31 downto 0)
    := EXC_RESET_VADDR + X"0000_0004";

  -- Exceptions
  signal IF_ID_IBE_exc      : std_logic := '0';

  
  ----------------------------------------------------------------------------  
  -- Instruction Decode Stage
  ----------------------------------------------------------------------------

  -- *Aliases* for fields in IF_ID_instruction:
  signal ID_opcode          : std_logic_vector(5 downto 0);
  signal ID_rs_num          : unsigned(4 downto 0);
  signal ID_rt_num          : unsigned(4 downto 0);
  signal ID_rd_num          : unsigned(4 downto 0);
  signal ID_shamt          : unsigned(4 downto 0);
  signal ID_funct           : std_logic_vector(5 downto 0);
  signal ID_immediate       : std_logic_vector(15 downto 0);
  signal ID_j_address       : unsigned(25 downto 0);
  
  -- Register File 
  signal ID_rgf_rs_data     : std_logic_vector(31 downto 0);
  signal ID_rgf_rt_data     : std_logic_vector(31 downto 0);
  signal ID_rgf_write_num   : unsigned(4 downto 0);
  signal ID_rgf_write_data  : std_logic_vector(31 downto 0);
  signal ID_rgf_write_en    : std_logic;

  -- From forwarding unit or register file
  signal ID_rs_data       : std_logic_vector(31 downto 0);
  signal ID_rt_data       : std_logic_vector(31 downto 0);

  -- Register Forwarding Mux
  signal ID_rs_fwd_mux      : rs_fwd_mux_type;
  signal ID_rt_fwd_mux      : rt_fwd_mux_type;
  
  -- Datapath enables from instruction decoder
  signal ID_do_link            : std_logic;
  signal ID_do_load            : std_logic;
  signal ID_do_store           : std_logic;
  signal ID_do_jmp_br          : std_logic;     -- used for PC and for CP0_BD
  
--  signal ID_do_reg_write       : std_logic;
  signal ID_do_reg_wr          : std_logic;
  signal ID_do_cp0_wr          : std_logic;
  signal ID_do_cp0_rd          : std_logic;
  signal ID_do_mdiv_op         : std_logic;
  signal ID_do_rs_read         : std_logic;
  signal ID_do_rt_read         : std_logic;
  signal ID_do_rfe             : std_logic;

  
  -- Opcodes from instruction decoder
  signal ID_alu_opcode      : std_logic_vector(3 downto 0);
  signal ID_gen_opcode      : std_logic_vector(3 downto 0);

  -- Datapath Muxes from instruction decoder
  -- Used here:
  signal ID_pc_jmp_br_mux      : pc_jmp_br_mux_type;
  signal ID_reg_dest_mux       : reg_dest_mux_type;
  -- Used later:
  signal ID_alu_in_B_mux       : alu_in_B_mux_type;
  signal ID_shamt_in_mux       : shamt_in_mux_type;
  signal ID_imm_ext_mux        : imm_ext_mux_type;
  signal ID_ex_out_mux         : ex_out_mux_type;
  signal ID_mem_out_mux        : mem_out_mux_type;
  signal ID_wb_out_mux         : wb_out_mux_type;
  
  -- Exceptions from instruction decoder
  signal ID_RI_exc         : std_logic; 
  signal ID_CpU_exc        : std_logic; 
  signal ID_SYS_exc        : std_logic; 
  signal ID_BRK_exc        : std_logic;
  
  -- Results from jump and branch units
  signal ID_branch_result   : unsigned(31 downto 0);  
  signal ID_jump_result     : unsigned(31 downto 0);
  signal ID_jump_align_error : std_logic;
  signal ID_AdEL_exc        : std_logic;
  
    -- Muxed outputs for do_link
  signal ID_rs_data_out     : std_logic_vector(31 downto 0);
  signal ID_immediate_out   : std_logic_vector(15 downto 0);

  signal ID_reg_dest_num    : unsigned(4 downto 0);

  
  ----------------------------------------------------------------------------  
  -- Instruction Decode / Execute Pipeline Registers
  ----------------------------------------------------------------------------

  -- Data
  signal ID_EX_pc_current   : unsigned(31 downto 0);
  signal ID_EX_immediate    : std_logic_vector(15 downto 0);
  signal ID_EX_reg_rs_data  : std_logic_vector(31 downto 0);
  signal ID_EX_reg_rt_data  : std_logic_vector(31 downto 0);
  signal ID_EX_reg_rs_num   : unsigned(4 downto 0);
  signal ID_EX_reg_rt_num   : unsigned(4 downto 0);
  signal ID_EX_reg_dest_num : unsigned(4 downto 0);
  signal ID_EX_shamt        : unsigned(4 downto 0);
  signal ID_EX_cp0_reg_num  : unsigned(4 downto 0);
  
  -- Opcodes
  signal ID_EX_alu_opcode   : std_logic_vector(3 downto 0);
  signal ID_EX_gen_opcode   : std_logic_vector(3 downto 0);
  
  -- Datapath Mux controls
  signal ID_EX_alu_in_B_mux : alu_in_B_mux_type;
  signal ID_EX_shamt_in_mux : shamt_in_mux_type;
  signal ID_EX_imm_ext_mux  : imm_ext_mux_type;
  signal ID_EX_ex_out_mux   : ex_out_mux_type;
  signal ID_EX_mem_out_mux  : mem_out_mux_type;
  signal ID_EX_wb_out_mux   : wb_out_mux_type;

  -- Datapath Enables
  signal ID_EX_do_jmp_br     : std_logic := '0';
  signal ID_EX_do_load       : std_logic := '0';
  signal ID_EX_do_store      : std_logic := '0';
  signal ID_EX_do_reg_wr     : std_logic := '0';
  signal ID_EX_do_cp0_wr     : std_logic := '0';
  signal ID_EX_do_cp0_rd     : std_logic := '0';
  signal ID_EX_do_mdiv_op    : std_logic := '0';
  signal ID_EX_do_rfe        : std_logic := '0';
  signal ID_EX_do_rs_read    : std_logic := '0';
  signal ID_EX_do_rt_read    : std_logic := '0';

  -- Exceptions
  signal ID_EX_IBE_exc      : std_logic := '0';  
  signal ID_EX_AdEL_exc     : std_logic := '0';
  signal ID_EX_SYS_exc      : std_logic := '0';
  signal ID_EX_BRK_exc      : std_logic := '0';
  signal ID_EX_RI_exc       : std_logic := '0';
  signal ID_EX_CpU_exc      : std_logic := '0';

  
  ----------------------------------------------------------------------------  
  -- Execute Stage
  ----------------------------------------------------------------------------
  
  -- Maybe from forwarding unit
  signal EX_rs_data         : std_logic_vector(31 downto 0);
  signal EX_rt_data         : std_logic_vector(31 downto 0);

  -- Register Forwarding Mux
  signal EX_rs_fwd_mux      : rs_fwd_mux_type;
  signal EX_rt_fwd_mux      : rt_fwd_mux_type;

  -- Functional unit inputs
  signal EX_alu_in_B        : std_logic_vector(31 downto 0);
  signal EX_shamt_in        : unsigned(4 downto 0);
  signal EX_imm_ext         : std_logic_vector(31 downto 0);
  
  -- Functional unit outputs
  signal EX_alu_result      : std_logic_vector(31 downto 0);
  signal EX_shift_result    : std_logic_vector(31 downto 0);
  signal EX_mdiv_result     : std_logic_vector(31 downto 0);
  signal EX_lui_result      : std_logic_vector(31 downto 0);
  signal EX_Ov_exc          : std_logic;

  -- Result is from ALU, Shift, or MDIV
  signal EX_result          : std_logic_vector(31 downto 0);

  
  ----------------------------------------------------------------------------  
  -- Execute / Memory Pipeline Registers
  ----------------------------------------------------------------------------  

  -- Data
  signal EX_MEM_result      : std_logic_vector(31 downto 0);
  signal EX_MEM_pc_current  : unsigned(31 downto 0);
  signal EX_MEM_reg_rt_data : std_logic_vector(31 downto 0);
  signal EX_MEM_reg_rs_num  : unsigned(4 downto 0);
  signal EX_MEM_reg_rt_num  : unsigned(4 downto 0);
  signal EX_MEM_cp0_reg_num : unsigned(4 downto 0);
  signal EX_MEM_reg_dest_num : unsigned(4 downto 0);

  -- Opcode for dmem
  signal EX_MEM_gen_opcode : std_logic_vector(3 downto 0);
  
  -- Datapath Mux controls
  signal EX_MEM_mem_out_mux  : mem_out_mux_type;
  signal EX_MEM_wb_out_mux   : wb_out_mux_type;
  signal EX_MEM_reg_dest_mux : reg_dest_mux_type;

  -- Datapath Enables
  signal EX_MEM_do_jmp_br     : std_logic := '0';
  signal EX_MEM_do_load       : std_logic := '0';
  signal EX_MEM_do_store      : std_logic := '0';
  signal EX_MEM_do_reg_wr     : std_logic := '0';
  signal EX_MEM_do_cp0_wr     : std_logic := '0';
  signal EX_MEM_do_cp0_rd     : std_logic := '0';
  signal EX_MEM_do_rfe        : std_logic := '0';
  signal EX_MEM_do_rs_read    : std_logic := '0';
  signal EX_MEM_do_rt_read    : std_logic := '0';


    -- Exceptions
  signal EX_MEM_IBE_exc      : std_logic := '0';  
  signal EX_MEM_AdEL_exc     : std_logic := '0';
  signal EX_MEM_SYS_exc      : std_logic := '0';
  signal EX_MEM_BRK_exc      : std_logic := '0';
  signal EX_MEM_RI_exc       : std_logic := '0';
  signal EX_MEM_CpU_exc      : std_logic := '0';
  signal EX_MEM_Ov_exc       : std_logic := '0';
  
  
  ----------------------------------------------------------------------------    
  -- Memory Access Stage
  ----------------------------------------------------------------------------

  -- Possibly from forwarding unit
  signal MEM_rs_data        : std_logic_vector(31 downto 0);
  signal MEM_rt_data        : std_logic_vector(31 downto 0);

    -- Register Forwarding Mux
  signal MEM_rs_fwd_mux      : rs_fwd_mux_type;
  signal MEM_rt_fwd_mux      : mem_rt_fwd_mux_type;


  -- EPC for CP0. Muxed based on do_jmp_br signal (BD)
  signal MEM_cp0_EPC_in      : unsigned(31 downto 0);

  -- Can be set by instruction mem error or dmem error:
  signal MEM_cp0_BadVaddr_in : unsigned(31 downto 0);
  signal MEM_AdES_exc        : std_logic;
  signal MEM_AdEL_exc        : std_logic;
  signal MEM_dmem_addr_error : std_logic;

  signal MEM_dmem_load_data  : std_logic_vector(31 downto 0);
  signal MEM_cp0_rd_data     : std_logic_vector(31 downto 0);  
  signal MEM_result          : std_logic_vector(31 downto 0);  
  
  ----------------------------------------------------------------------------    
  -- Memory / Writeback Pipeline Registers
  ----------------------------------------------------------------------------
  signal MEM_WB_pc_current  : unsigned(31 downto 0);
  signal MEM_WB_ex_result   : std_logic_vector(31 downto 0);
  signal MEM_WB_mem_result  : std_logic_vector(31 downto 0);
  signal MEM_WB_rgf_dest_num  : unsigned(4 downto 0);
  signal MEM_WB_rgf_wr_en   : std_logic := '0';

  signal MEM_WB_wb_out_mux  : wb_out_mux_type;
  
  -- for BD bit 
  signal MEM_WB_do_jmp_br : std_logic;
  

  
  ----------------------------------------------------------------------------    
  -- Writeback Stage
  ----------------------------------------------------------------------------  

  signal WB_out             : std_logic_vector(31 downto 0);
  signal WB_out_mux         : WB_out_mux_type;

  
  -- GLOBAL STATE: may be read or written by any pipe stage or module
  --
  ----------------------------------------------------------------------------  
  -- Pipeline Stall or Flush:
  ----------------------------------------------------------------------------
  signal PC_enable            : std_logic;
  signal IF_ID_enable         : std_logic;
  signal ID_EX_enable         : std_logic;
  signal EX_MEM_enable        : std_logic;
  signal MEM_WB_enable        : std_logic;

  -- rename this, it's silly (PC uses it too, but that's not a flush)
  signal exception_flush      : std_logic;
  signal IF_ID_flush          : std_logic;
  signal ID_EX_flush          : std_logic;
  signal EX_MEM_flush         : std_logic;
  signal MEM_WB_flush         : std_logic;

  signal exc_vector_mux       : exc_vector_mux_type;
  signal boot_exc_vectors     : std_logic;

  signal mdiv_stall           : std_logic;
  signal load_use_stall       : std_logic;
  signal jump_branch_stall    : std_logic;
  
  ----------------------------------------------------------------------------
  -- CP0 state (exported register bits)
  ----------------------------------------------------------------------------
  signal big_endian_mode    : std_logic;
  signal kernel_mode        : std_logic;

  
begin

  ----------------------------------------------------------------------------
  -- INSTRUCTION FETCH STAGE
  ----------------------------------------------------------------------------
  -- TODO:
  --  Is ibus_rd_en necessary? (Currently unused)
  --  Relocated IF_AdEL_exc to ID stage for performance.
  --  Could optimize PC logic: boot vectors can be registered (very small gain)

  -- Read next instruction
--  ibus_addr <= pc_ibus_rd_addr;
  ibus_rd_en <= '1';

  mips_imem_interface: entity work.imem_interface
    port map (instruction => IF_instruction,
              big_endian_mode => big_endian_mode,
              ibus_rd_data => ibus_rd_data
              );

  
  -- Program counter next muxes
  -- Exceptions override this (within PC module)
  IF_pc_next <=
    ID_jump_result   when ID_do_jmp_br = '1' and ID_pc_jmp_br_mux = DO_JUMP else
    ID_branch_result when ID_do_jmp_br = '1' and ID_pc_jmp_br_mux = DO_BRANCH else
    IF_pc_plus_4;


  mips_pc: entity work.program_counter
    port map (clk => clk,
              reset => reset,
              exception_flush => exception_flush,
              exc_vector_mux => exc_vector_mux,
              boot_exc_vectors => boot_exc_vectors,
              pc_next => IF_pc_next,
              pc_current => IF_pc_current,
              pc_plus_4 => IF_pc_plus_4,
              pc_ibus_addr => ibus_addr,              
              pc_enable => pc_enable
              );

  
  -- IF_ID pipeline register writes
  process (clk)
  begin
    if rising_edge(clk) then
      
      if IF_ID_enable = '1' then
        -- Data
        IF_ID_pc_current <= IF_pc_current;
        IF_ID_pc_plus_4 <= IF_pc_plus_4;
        IF_ID_instruction <= IF_instruction;
        -- Exceptions
        IF_ID_IBE_exc <= ibus_fault;
      end if;
      
      if IF_ID_flush = '1' then
        IF_ID_IBE_exc <= '0';

        -- Load NOPs during reset to put the pipeline in a known state
        IF_ID_instruction <= (others => '0');
      end if;
      
    end if;
  end process;
  

  ----------------------------------------------------------------------------
  -- INSTRUCTION DECODE STAGE
  ----------------------------------------------------------------------------
  -- Registers read in this stage go through several muxes. 
  -- 1. Forwarding mux from EX_MEM or MEM_WB   -> ID_rs_data, ID_rt_data
  -- 2. Link mux (for JAL, BGTZAL, etc)        -> ID_rs_data_out, ID_immediate_out
  
  
  -- Aliases for convenience
  ID_opcode <= IF_ID_instruction(31 downto 26);
  ID_rs_num <= unsigned(IF_ID_instruction(25 downto 21));
  ID_rt_num <= unsigned(IF_ID_instruction(20 downto 16));
  ID_rd_num <= unsigned(IF_ID_instruction(15 downto 11));
  ID_shamt <= unsigned(IF_ID_instruction(10 downto 6));
  ID_immediate <= IF_ID_instruction(15 downto 0);
  ID_j_address <= unsigned(IF_ID_instruction(25 downto 0));


  -- Forwarding unit mux
  with ID_rs_fwd_mux select ID_rs_data <=
    EX_MEM_result       when FWD_RS_FROM_EX,
    WB_out              when FWD_RS_FROM_MEM,
    ID_rgf_rs_data      when RS_LOCAL;

  with ID_rt_fwd_mux select ID_rt_data <=
    EX_MEM_result       when FWD_RT_FROM_EX,
    WB_out              when FWD_RT_FROM_MEM,
    ID_rgf_rt_data      when RT_LOCAL;
  
  
  mips_regfile: entity work.register_file
    port map (clk => clk,
              rgf_rs_num => ID_rs_num,
              rgf_rt_num => ID_rt_num,
              rgf_rs_data => ID_rgf_rs_data,
              rgf_rt_data => ID_rgf_rt_data,
              rgf_write_num => MEM_WB_rgf_dest_num,
              rgf_write_data => WB_out,
              rgf_write_en => MEM_WB_rgf_wr_en
              );

  mips_instruction_decode: entity work.instr_decoder
    port map (instruction => IF_ID_instruction,
              -- Datapath muxes
              pc_jmp_br_mux => ID_pc_jmp_br_mux,
              alu_in_B_mux => ID_alu_in_B_mux,
              shamt_in_mux => ID_shamt_in_mux,
              imm_ext_mux => ID_imm_ext_mux,
              ex_out_mux => ID_ex_out_mux,
              mem_out_mux => ID_mem_out_mux,
              wb_out_mux => ID_wb_out_mux,
              reg_dest_mux => ID_reg_dest_mux,
              -- Datapath enables
              do_link => ID_do_link,
              do_load => ID_do_load,
              do_store => ID_do_store,
              do_jump_branch => ID_do_jmp_br,
              do_reg_write => ID_do_reg_wr,
              do_cp0_wr => ID_do_cp0_wr,
              do_cp0_rd => ID_do_cp0_rd,
              do_mdiv_op => ID_do_mdiv_op,
              do_rs_read => ID_do_rs_read,
              do_rt_read => ID_do_rt_read,
              do_rfe => ID_do_rfe,
              -- Opcodes
              alu_opcode => ID_alu_opcode,
              gen_opcode => ID_gen_opcode,
              -- Exceptions passed to CP0
              RI_exception => ID_RI_exc,
              CpU_exception => ID_CpU_exc,
              SYS_exception => ID_SYS_exc,
              BRK_exception => ID_BRK_exc
              );

  mips_branch_unit: entity work.branch_unit
    port map (gen_opcode => ID_gen_opcode,
              branch_pc_plus_4 => IF_ID_pc_plus_4,
              branch_rs_reg_data => unsigned(ID_rs_data),
              branch_rt_reg_data => unsigned(ID_rt_data),
              branch_immediate => unsigned(ID_immediate),
              branch_pc_result => ID_branch_result
              );

  mips_jump_unit: entity work.jump_unit
    port map (gen_opcode => ID_gen_opcode,
              jump_pc4_top => IF_ID_pc_plus_4(31 downto 28),
              jump_address => ID_j_address,
              jump_rs_reg_data => unsigned(ID_rs_data),
              jump_result => ID_jump_result,
              jump_align_error => ID_jump_align_error
              );

  -- Should be OK to check illegal user kseg0 access here, since we can't
  -- modify CPU state before the exception checkpoint in the MEM stage
  ID_AdEL_exc <=
    '1' when IF_ID_pc_current(31) = '1' and kernel_mode = '0' else
    '1' when (ID_jump_align_error and ID_do_jmp_br) = '1'
             and ID_pc_jmp_br_mux = DO_JUMP else
    '0';
  
  -- Select the instruction field that has the destination register number
  with ID_reg_dest_mux select ID_reg_dest_num <=
    ID_rt_num           when USE_RT_REG,
    ID_rd_num           when USE_RD_REG,
    "11111"             when USE_LINK_REG;

  
  -- For JAL, BLTZAL, etc, use ALU to compute PC+8
  link_mux_proc:
  process (ID_do_link, ID_immediate, ID_rs_data, IF_ID_pc_current)
  begin
    if ID_do_link = '1' then
      -- Special ALU inputs for do_link
      ID_immediate_out <= X"0008";
      ID_rs_data_out   <= std_logic_vector(IF_ID_pc_current);
    else
      -- Normal ALU inputs otherwise
      ID_immediate_out <= ID_immediate;
      ID_rs_data_out   <= ID_rs_data;
    end if;
  end process;
  
  
  -- ID_EX pipeline register writes
  process (clk)
  begin
    if rising_edge(clk) then
      if ID_EX_enable = '1' then
        -- Data
        ID_EX_pc_current <= IF_ID_pc_current;
        ID_EX_immediate <= ID_immediate_out;
        ID_EX_reg_rs_data <= ID_rs_data_out;
        ID_EX_reg_rt_data <= ID_rt_data;
        ID_EX_reg_rs_num <= ID_rs_num;
        ID_EX_reg_rt_num <= ID_rt_num;
        ID_EX_reg_dest_num <= ID_reg_dest_num;
        ID_EX_shamt <= ID_shamt;
        ID_EX_cp0_reg_num <= ID_rd_num;
        
        -- Opcodes
        ID_EX_alu_opcode <= ID_alu_opcode;
        ID_EX_gen_opcode <= ID_gen_opcode;
        
        -- Datapath Muxes
        ID_EX_alu_in_B_mux <= ID_alu_in_B_mux;
        ID_EX_shamt_in_mux <= ID_shamt_in_mux;
        ID_EX_imm_ext_mux  <= ID_imm_ext_mux;
        ID_EX_ex_out_mux   <= ID_ex_out_mux;
        ID_EX_mem_out_mux  <= ID_mem_out_mux;
        ID_EX_wb_out_mux   <= ID_wb_out_mux;

        -- Datapath Enables
        ID_EX_do_jmp_br <= ID_do_jmp_br;
        ID_EX_do_load <= ID_do_load;
        ID_EX_do_store <= ID_do_store;
        ID_EX_do_reg_wr <= ID_do_reg_wr;
        ID_EX_do_cp0_wr <= ID_do_cp0_wr;
        ID_EX_do_cp0_rd <= ID_do_cp0_rd;
        ID_EX_do_mdiv_op <= ID_do_mdiv_op;
        ID_EX_do_rfe <= ID_do_rfe;
        ID_EX_do_rs_read <= ID_do_rs_read;
        ID_EX_do_rt_read <= ID_do_rt_read;
        
        -- Exceptions 
        ID_EX_IBE_exc <= IF_ID_IBE_exc;
        ID_EX_AdEL_exc <= ID_AdEL_exc;
        ID_EX_SYS_exc <= ID_SYS_exc;
        ID_EX_BRK_exc <= ID_BRK_exc;
        ID_EX_RI_exc <= ID_RI_exc;
        ID_EX_CpU_exc <= ID_CpU_exc;
      end if;

      -- flush control registers and exceptions
      if ID_EX_flush = '1' then
        -- Datapath Enables
        ID_EX_do_jmp_br <= '0';
        ID_EX_do_load <= '0';
        ID_EX_do_store <= '0';
        ID_EX_do_reg_wr <= '0';
        ID_EX_do_cp0_wr <= '0';
        ID_EX_do_cp0_rd <= '0';
        ID_EX_do_mdiv_op <= '0';
        ID_EX_do_rfe <= '0';
        
        -- Exceptions
        ID_EX_IBE_exc <= '0';
        ID_EX_AdEL_exc <= '0';
        ID_EX_SYS_exc <= '0';
        ID_EX_BRK_exc <= '0';
        ID_EX_RI_exc <= '0';
        ID_EX_CpU_exc <= '0';
      end if;
    end if;
  end process;


  
  ---------------------------------------------------------------------------- 
  -- EXECUTE STAGE
  ----------------------------------------------------------------------------
  -- Looks like a very long critical path from ALU input muxes to EX_result mux...
  -- ALU inputs: X-Y -> input_A - input_B -> rs_data - rt_data

  -- Forwarding unit mux:
  with EX_rs_fwd_mux select EX_rs_data <=
    EX_MEM_result       when FWD_RS_FROM_EX,
    WB_out              when FWD_RS_FROM_MEM,
    ID_EX_reg_rs_data   when RS_LOCAL;

  with EX_rt_fwd_mux select EX_rt_data <=
    EX_MEM_result       when FWD_RT_FROM_EX,
    WB_out              when FWD_RT_FROM_MEM,
    ID_EX_reg_rt_data   when RT_LOCAL;

  -- Arithmetic input muxes:
  with ID_EX_alu_in_B_mux select EX_alu_in_B <=
    EX_rt_data          when USE_RT_DATA,
    EX_imm_ext          when USE_IMMEDIATE;

  with ID_EX_shamt_in_mux select EX_shamt_in <=
    unsigned(EX_rs_data(4 downto 0))    when USE_RS_DATA,
    ID_EX_shamt                         when USE_SHAMT_FIELD;

  EX_lui_result <= ID_EX_immediate & X"0000";
  
  with ID_EX_imm_ext_mux select EX_imm_ext <=
    (31 downto 16 => ID_EX_immediate(15)) & ID_EX_immediate when DO_SIGN_EXTEND,
    (31 downto 16 => '0')                 & ID_EX_immediate when DO_ZERO_EXTEND;

  
  mips_alu: entity work.alu
    port map (alu_opcode => ID_EX_alu_opcode,
              alu_input_A => EX_rs_data,
              alu_input_B => EX_alu_in_B,
              alu_result => EX_alu_result,
              alu_overflow => EX_Ov_exc
              );

  mips_barrel_shifter: entity work.barrel_shifter
    port map (shift_input => EX_rt_data,
              shift_amount => EX_shamt_in,
              gen_opcode => ID_EX_gen_opcode,
              shift_result => EX_shift_result
              );

  mips_mult_div: entity work.mult_div
    port map (clk => clk,
              gen_opcode => ID_EX_gen_opcode,
              do_mdiv_op => ID_EX_do_mdiv_op,
              rs_reg_data => EX_rs_data,
              rt_reg_data => EX_rt_data,
              mdiv_result => EX_mdiv_result,
              mdiv_stall => mdiv_stall
              );

  with ID_EX_ex_out_mux select EX_result <=
    EX_alu_result       when USE_ALU_RESULT,
    EX_shift_result     when USE_SHIFT_RESULT,
    EX_mdiv_result      when USE_MDIV_RESULT,
    EX_lui_result       when USE_LUI_RESULT;


  -- EX_MEM pipeline register writes
  process (clk)
  begin
    if rising_edge(clk) then
      if EX_MEM_enable = '1' then
        -- Data
        EX_MEM_pc_current <= ID_EX_pc_current;
        EX_MEM_result <= EX_result;
        EX_MEM_reg_rt_data <= ID_EX_reg_rt_data;
        EX_MEM_reg_rs_num <= ID_EX_reg_rs_num;
        EX_MEM_reg_rt_num <= ID_EX_reg_rt_num;
        EX_MEM_reg_dest_num <= ID_EX_reg_dest_num;
        EX_MEM_cp0_reg_num <= ID_EX_cp0_reg_num;

        EX_MEM_gen_opcode <= ID_EX_gen_opcode;
        
        -- Datapath Mux Controls
        EX_MEM_mem_out_mux <= ID_EX_mem_out_mux;
        EX_MEM_wb_out_mux <= ID_EX_wb_out_mux;

        -- Datapath Enables
        EX_MEM_do_jmp_br <= ID_EX_do_jmp_br;
        EX_MEM_do_load <= ID_EX_do_load;
        EX_MEM_do_store <= ID_EX_do_store;
        EX_MEM_do_reg_wr <= ID_EX_do_reg_wr;
        EX_MEM_do_cp0_wr <= ID_EX_do_cp0_wr;
        EX_MEM_do_cp0_rd <= ID_EX_do_cp0_rd;
        EX_MEM_do_rfe <= ID_EX_do_rfe;
        EX_MEM_do_rs_read <= ID_EX_do_rs_read;
        EX_MEM_do_rt_read <= ID_EX_do_rt_read;
        
        -- Exceptions
        EX_MEM_IBE_exc <= ID_EX_IBE_exc;
        EX_MEM_AdEL_exc <= ID_EX_AdEL_exc;
        EX_MEM_SYS_exc <= ID_EX_SYS_exc;
        EX_MEM_BRK_exc <= ID_EX_BRK_exc;
        EX_MEM_RI_exc <= ID_EX_RI_exc;
        EX_MEM_CpU_exc <= ID_EX_CpU_exc;
        EX_MEM_Ov_exc <= EX_Ov_exc;
      end if;

      -- flush control registers and exceptions
      if EX_MEM_flush = '1' then
        -- Datapath Enables
        EX_MEM_do_jmp_br <= '0';
        EX_MEM_do_load <= '0';
        EX_MEM_do_store <= '0';
        EX_MEM_do_reg_wr <= '0';
        EX_MEM_do_cp0_wr <= '0';
        EX_MEM_do_cp0_rd <= '0';
        EX_MEM_do_rfe <= '0';
        
        -- Exceptions
        EX_MEM_IBE_exc <= '0';
        EX_MEM_AdEL_exc <= '0';
        EX_MEM_SYS_exc <= '0';
        EX_MEM_BRK_exc <= '0';
        EX_MEM_RI_exc <= '0';
        EX_MEM_CpU_exc <= '0';
        EX_MEM_Ov_exc <= '0';
      end if;
    end if;
  end process;

  
  ----------------------------------------------------------------------------
  -- MEMORY ACCESS STAGE
  ----------------------------------------------------------------------------
  -- Should rd_en and wr_en be gated with MEM_WB_enable ?
  -- Forwarding from WB_out_mux is going to slow us down...
  
  -- Data bus access
  dbus_addr <= unsigned(EX_MEM_result(31 downto 0));
  dbus_rd_en <= EX_MEM_do_load;
  dbus_wr_en <= EX_MEM_do_store;


  -- Forwarding unit mux
  with MEM_rt_fwd_mux select MEM_rt_data <=
    WB_out              when FWD_RT_FROM_MEM,
    EX_MEM_reg_rt_data  when RT_LOCAL;
  

  mips_dmem_interface: entity work.dmem_interface
    port map (gen_opcode => EX_MEM_gen_opcode,
              dmem_addr_error => MEM_dmem_addr_error,       -- alignment check
              big_endian_mode => big_endian_mode,
              
              dmem_address => unsigned(EX_MEM_result),
              dmem_load_data => MEM_dmem_load_data,
              dmem_store_data => MEM_rt_data,
              
              dbus_lane_en => dbus_lane_en,
              dbus_addr => dbus_addr,
              dbus_rd_data => dbus_rd_data,
              dbus_wr_data => dbus_wr_data
              );

  
  -- EPC must point to branch/jump when instruction in delay slot
  -- suffers exception. Additionally, BD bit in cp0_CR will be set.
  MEM_cp0_EPC_in <=
    EX_MEM_pc_current    when MEM_WB_do_jmp_br = '0' else
    MEM_WB_pc_current    when MEM_WB_do_jmp_br = '1';

  -- Address error on instruction load happens earlier, so it takes precedence.
  -- If we get an address error from DMEM, the alu_result will be the address.
  MEM_cp0_BadVaddr_in <=
    EX_MEM_pc_current    when EX_MEM_AdEL_exc = '1' else
    unsigned(EX_MEM_result);
  
  -- Address error on load can come from instruction memory, alignment error
  -- detected in dmem_interface, or illegal user mode access, detected here.
  MEM_AdEL_exc <=
    EX_MEM_AdEL_Exc or (MEM_dmem_addr_error and EX_MEM_do_load) or
    (EX_MEM_result(31) and not kernel_mode and EX_MEM_do_load);

  -- Address error on store can come from an alignment error detected in
  -- dmem_interface, or illegal user mode access, detected here.
  MEM_AdES_exc <=
    (MEM_dmem_addr_error and EX_MEM_do_store) or
    (EX_MEM_result(31) and not kernel_mode and EX_MEM_do_store);
  
  
  mips_coprocessor_0: entity work.coproc0
    port map (clk => clk,
              reset => reset,
              cp0_wr_data => MEM_rt_data,
              cp0_rd_data => MEM_cp0_rd_data,
              cp0_reg_num => EX_MEM_cp0_reg_num,
              cp0_wr_en => EX_MEM_do_cp0_wr,
              cp0_do_rfe => EX_MEM_do_rfe,
              cp0_EPC_in => std_logic_vector(MEM_cp0_EPC_in),
              cp0_BadVaddr_in => std_logic_vector(MEM_cp0_BadVaddr_in),
              cp0_BD_in => MEM_WB_do_jmp_br,

              Mod_exception => '0',
              TLBL_exception => '0',
              TLBS_exception => '0',
              AdEL_exception => MEM_AdEL_exc,
              AdES_exception => MEM_AdES_exc,
              IBE_exception => EX_MEM_IBE_exc,
              DBE_exception => dbus_fault,
              SYS_exception => EX_MEM_SYS_exc,
              BRK_exception => EX_MEM_BRK_exc,
              RI_exception => EX_MEM_RI_exc,
              CpU_exception => EX_MEM_CpU_exc,
              Ov_exception => EX_MEM_Ov_exc,

              -- Globals
              hw_ext_interrupts => hw_ext_interrupts,
              big_endian_mode => big_endian_mode,
              kernel_mode => kernel_mode,
              boot_exc_vectors => boot_exc_vectors,
              exc_vector_mux => exc_vector_mux,
              exception_flush => exception_flush
              );


  -- For viewing on an oscop / logic analyzer [debug/bringup only]:
  exception_out <= exception_flush;

  
  with EX_MEM_mem_out_mux select MEM_result <=
    MEM_dmem_load_data      when USE_DMEM_RESULT,
    MEM_cp0_rd_data         when USE_CP0_RESULT;

  
  -- MEM_WB pipeline register writes
  process (clk)
  begin
    if rising_edge(clk) then
      if MEM_WB_enable = '1' then
        MEM_WB_pc_current <= EX_MEM_pc_current;
        MEM_WB_ex_result <= EX_MEM_result;
        MEM_WB_mem_result <= MEM_result;
        MEM_WB_rgf_dest_num <= EX_MEM_reg_dest_num;
        MEM_WB_rgf_wr_en <= EX_MEM_do_reg_wr;
        MEM_WB_do_jmp_br <= EX_MEM_do_jmp_br;
        MEM_WB_wb_out_mux <= EX_MEM_wb_out_mux;
      end if;
      
      -- flush control registers and exceptions
      if MEM_WB_flush = '1' then
        MEM_WB_rgf_wr_en <= '0';
      end if;
    end if;
  end process;

  
  ----------------------------------------------------------------------------
  -- WRITEBACK STAGE
  ----------------------------------------------------------------------------

  with MEM_WB_wb_out_mux select WB_out <=
    MEM_WB_mem_result  when USE_MEM_RESULT,
    MEM_WB_ex_result   when USE_EX_RESULT;



  ----------------------------------------------------------------------------
  -- PIPELINE STALL 
  ----------------------------------------------------------------------------
  -- Multiply/divide pipeline lock is detected by MDIV unit, not hazard unit
  
  mips_hazard_unit: entity work.hazard_unit
    port map (EX_MEM_do_load => EX_MEM_do_load,
              EX_MEM_do_cp0_rd => EX_MEM_do_cp0_rd,
              EX_MEM_reg_dest_num => EX_MEM_reg_dest_num,
              EX_do_rs_read => ID_EX_do_rs_read,
              EX_do_rt_read => ID_EX_do_rt_read,
              ID_EX_rs_num => ID_EX_reg_rs_num,
              ID_EX_rt_num => ID_EX_reg_rt_num,
              ID_rs_num => ID_rs_num,
              ID_rt_num => ID_rt_num,
              ID_EX_do_reg_write => ID_EX_do_reg_wr,
              ID_EX_reg_dest_num => ID_EX_reg_dest_num,
              ID_do_jump_branch => ID_do_jmp_br,
              ID_rs_reg_num => ID_rs_num,
              ID_do_rs_read => ID_do_rs_read,
              ID_rt_reg_num => ID_rt_num,
              ID_do_rt_read => ID_do_rt_read,
              
              load_use_stall => load_use_stall,
              jump_branch_stall => jump_branch_stall
              );

  stall_flush_proc:
  process (reset, load_use_stall, mdiv_stall, jump_branch_stall, exception_flush)
  begin
    
    PC_enable <= '1';
    
    IF_ID_enable <= '1';
    ID_EX_enable <= '1';
    EX_MEM_enable <= '1';
    MEM_WB_enable <= '1';
    
    IF_ID_flush <= '0';
    ID_EX_flush <= '0';
    EX_MEM_flush <= '0';
    MEM_WB_flush <= '0';
    
    if load_use_stall = '1' then
      PC_enable <= '0';
      IF_ID_enable <= '0';
      ID_EX_enable <= '0';
      EX_MEM_flush <= '1';
    end if;

    if mdiv_stall = '1' then
      PC_enable <= '0';
      IF_ID_enable <= '0';
      ID_EX_enable <= '0';
      EX_MEM_flush <= '1';
    end if;

    -- Should ID_EX get flushed here?
    if jump_branch_stall = '1' then
      PC_enable <= '0';
      IF_ID_enable <= '0';
    end if;

    if (reset or exception_flush) = '1' then
      IF_ID_flush <= '1';
      ID_EX_flush <= '1';
      EX_MEM_flush <= '1';
      MEM_WB_flush <= '1';
    end if;
    
  end process;
  

  
  ----------------------------------------------------------------------------
  -- FORWARDING
  ----------------------------------------------------------------------------
  
  mips_fwd_unit: entity work.fwd_unit
    port map (
      -- input
      EX_MEM_rgf_wr_en => EX_MEM_do_reg_wr,
      MEM_WB_rgf_wr_en => MEM_WB_rgf_wr_en,
      EX_MEM_reg_dest_num => EX_MEM_reg_dest_num,
      MEM_WB_reg_dest_num => MEM_WB_rgf_dest_num,
      ID_rs_reg_num => ID_rs_num,
      ID_rt_reg_num => ID_rt_num,
      EX_rs_reg_num => ID_EX_reg_rs_num,
      EX_rt_reg_num => ID_EX_reg_rt_num,
      -- output
      MEM_rt_reg_num => EX_MEM_reg_rt_num,
      ID_rs_fwd_mux => ID_rs_fwd_mux,
      ID_rt_fwd_mux => ID_rt_fwd_mux,
      EX_rs_fwd_mux => EX_rs_fwd_mux,
      EX_rt_fwd_mux => EX_rt_fwd_mux,
      MEM_rt_fwd_mux => MEM_rt_fwd_mux
      );
  
   
end arch;


-- Exceptions: Can be generated in IF, ID, EX, or MEM, will be passed down the
-- pipeline without affecting anything else, because no CPU state is committed
-- until MEM or WB perform writes. In the MEM stage, 'exception_flush' gets
-- asserted when an exception is detected. The enable signals (do_store, reg
-- write, etc) for the current instruction and all previous ones are then zeroed.
-- Also, the 'exception' input to the PC is asserted.
-- 
-- Stalls: The Hazard Unit asserts 'load_use_stall' and 'jump_branch_stall',
-- while the multiply/divide unit asserts 'mdiv_stall'. In each case, the 
-- write enable signals for the PC and pipeline registers prior to the stall
-- are zeroed as long as the hazard persists. The pipeline register where the
-- stall occurs is injected with NOPs while the hazard persists. NOPs are
-- accomplished simply by zeroing all exceptinos and write enables.
--
----------------------------------------------------------------------------
-- Exception             Registers Flushed
----------------------------------------------------------------------------
-- Any                   ID_EX, EX_MEM
-- 
-- *IF_ID loads the exception vector. MEM_WB will be loaded with a NOP.
--
----------------------------------------------------------------------------
-- Stall Condition       Registers Locked   Registers NOP'd
----------------------------------------------------------------------------
-- Load Use Stall:       PC, IF_ID, ID_EX   EX_MEM
-- MDIV stall:           PC, IF_ID, ID_EX   EX_MEM
-- Jump Branch Stall:    PC, IF_ID          ID_EX ??? ...Does it matter?
--
-- D-Cache Miss (Load):  ???                ???
-- I-Cache Miss:         PC                 IF_ID
-- 
-- *A D-Cache miss should not stall unless the write buffer is full and the
-- next instruction is a store, or we missed on a load and the next instruction
-- uses the loaded data.
