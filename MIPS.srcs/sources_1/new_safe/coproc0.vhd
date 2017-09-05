library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

-- Basic CP0 to support interrupts, but no caches or TLB
--
-- *** Counter / Compare match is HARDWIRED to hw_interrupt_0
----------------------------------------------------------------------
-- Feature support:
----------------------------------------------------------------------
-- Full interrupt support:
--  Global interrupt enable and 8 interrupt sources with mask / pending bits.
--  KU/IE bits pushed into 2-entry HW 'stack' on interrupt, popped on RFE.
--  Cause register.
--  EPC register.
--  BadVaddr register.
--  Counter / Compare interrupt
--


entity coproc0 is
  port (clk               : in std_logic;
        reset             : in std_logic;
        
        -- For software read/write of a CP0 register
        cp0_wr_data       : in std_logic_vector(31 downto 0);
        cp0_rd_data       : out std_logic_vector(31 downto 0);
        cp0_reg_num       : in unsigned(4 downto 0);
        cp0_wr_en         : in std_logic;
        cp0_do_rfe        : in std_logic;
        
        -- For hardware write of SR, CR, EPC, badvaddr, during exception
        cp0_EPC_in        : in std_logic_vector(31 downto 0);
        cp0_BadVaddr_in   : in std_logic_vector(31 downto 0);
        cp0_BD_in         : in std_logic;
        
        -- External HW interrupt lines (devices like UART, timer, etc)
        -- NOTE: these need to be synchronous! it should go through an FF or two
        hw_ext_interrupts : in std_logic_vector(5 downto 0);

        -- Unlike interrupts, exceptions are non-maskable
        Mod_exception     : in std_logic;
        TLBL_exception    : in std_logic;
        TLBS_exception    : in std_logic;
        AdEL_exception    : in std_logic;
        AdES_exception    : in std_logic;
        IBE_exception     : in std_logic;
        DBE_exception     : in std_logic;
        SYS_exception     : in std_logic;
        BRK_exception     : in std_logic;
        RI_exception      : in std_logic;
        CpU_exception     : in std_logic;
        Ov_exception      : in std_logic;

        -- Exported state
        big_endian_mode   : out std_logic;
        kernel_mode       : out std_logic;
        boot_exc_vectors  : out std_logic;
        exc_vector_mux    : out exc_vector_mux_type;

        -- Flush pipeline and jump to exception vector!
        exception_flush   : out std_logic
    );
end coproc0;

architecture arch of coproc0 is

  
  
  -- Processor ID Register (Read only)
  constant PRId_Imp_field  : std_logic_vector(7 downto 0) := x"FF";
  constant PRId_Rev_field  : std_logic_vector(7 downto 0) := x"01";
  constant PRId_constant   : std_logic_vector(31 downto 0)
    := X"0000" & PRId_Imp_field & PRId_Rev_field;

  -- Actual registers:
  ----------------------------------------------------------------------
  signal EPC_reg           : std_logic_vector(31 downto 0) := (others => '0');
  signal Cause_reg         : std_logic_vector(31 downto 0) := (others => '0');
  signal Status_reg        : std_logic_vector(31 downto 0)
    := (22 => '1', 5 => '1', others => '0');
  signal BadVaddr_reg      : std_logic_vector(31 downto 0) := (others => '0');
  signal Count_reg         : unsigned(23 downto 0) := (others => '0');
  signal Compare_reg       : unsigned(23 downto 0) := (others => '1');

  
  -- Register Aliases:
  ----------------------------------------------------------------------
  -- BD: Branch Delay Slot
  alias Cause_BD      : std_logic is Cause_reg(31);
  -- CE: Coprocessor Error Number
  alias Cause_CE      : std_logic_vector(1 downto 0) is Cause_reg(29 downto 28);
  -- IP: Interrupt Pending:
  alias Cause_IP      : std_logic_vector(7 downto 0) is Cause_reg(15 downto 8);
  -- ExcCode: Exception Code
  alias Cause_EXcCode : std_logic_vector(4 downto 0) is Cause_reg(6 downto 2);

  alias Status_CU3  : std_logic is Status_reg(31);  -- 31 Coprocessor 3 usable
  alias Status_CU2  : std_logic is Status_reg(30);  -- 30 Coprocessor 2 usable
  alias Status_CU1  : std_logic is Status_reg(29);  -- 29 Coprocessor 1 usable
  alias Status_CU0  : std_logic is Status_reg(28);  -- 28 Coprocessor 0 usable
  alias Status_RE   : std_logic is Status_reg(25);  -- 25 Reverse Endianness
  alias Status_BEV  : std_logic is Status_reg(22);  -- 22 Boot Exception Vectors
  alias Status_TS   : std_logic is Status_reg(21);  -- 21 TLB Shutdown
  alias Status_PE   : std_logic is Status_reg(20);  -- 20 (Cache) Parity Error
  alias Status_CM   : std_logic is Status_reg(19);  -- 19 Cache Management ???
  alias Status_PZ   : std_logic is Status_reg(18);  -- 18 (Cache) Parity Zeroed
  alias Status_SwC  : std_logic is Status_reg(17);  -- 17 Swap Caches
  alias Status_IsC  : std_logic is Status_reg(16);  -- 16 Isolate (Data) Cache
  -- IM: Interrupt Mask
  alias Status_IM   : std_logic_vector(7 downto 0) is Status_REG(15 downto 8);
  alias Status_KUc  : std_logic is Status_reg(5);   --  5 Kernel Mode (1)
  alias Status_IEc  : std_logic is Status_reg(4);   --  4 Interrupts Enabled (1)
  alias Status_KUp  : std_logic is Status_reg(3);   --  3 KU previous
  alias Status_IEp  : std_logic is Status_reg(2);   --  2 IE previous
  alias Status_KUo  : std_logic is Status_reg(1);   --  1 KU old
  alias Status_IEo  : std_logic is Status_reg(0);   --  0 IE old


  -- Combinatorial logical (used internally)
  ----------------------------------------------------------------------
  signal any_interrupt     : std_logic;
  signal any_exception     : std_logic;
  signal ExcCode_next      : unsigned(4 downto 0);
  signal start_exception   : std_logic;
  signal do_cp0_write      : std_logic;
  signal compare_match     : std_logic;  -- count/compare register match
  
  -- consider removing:
  --  signal exception_slv     : std_logic_vector(12 downto 0);
  
  -- Exception can be generated inside CP0 by illegal user access attempt
  signal CpU_exc_internal  : std_logic;
  
  -- register select (sparse decoder)
  signal Status_reg_sel    : std_logic;
  signal Cause_reg_sel     : std_logic;
  signal EPC_reg_sel       : std_logic;
  signal BadVaddr_reg_sel  : std_logic;
  signal Count_reg_sel     : std_logic;
  signal Compare_reg_sel   : std_logic;


  -- The synthesizer seems to be uneccessarily messing with these bits...
  --attribute dont_touch : string;
  --attribute dont_touch of Status_reg      : signal is "true";
  --attribute dont_touch of Cause_reg       : signal is "true";
  --attribute dont_touch of start_exception : signal is "true";

  
begin

  ----------------------------------------------------------------------------  
  -- Exported State
  ----------------------------------------------------------------------------

  -- We're big endian unless reversed with the Status_RE bit.
  big_endian_mode <= not Status_RE;

  -- Important: memory accesses with the MSB of the address set cause an exception
  -- when not in kernel mode.
  kernel_mode <= Status_KUc;

  -- Technically this is irrelevant as long as we do not have caches...
  boot_exc_vectors <= Status_BEV;

  -- Flush all pipeline register control signals
  exception_flush <= start_exception;

  
  ----------------------------------------------------------------------------  
  -- Interrupts and Exceptions
  ----------------------------------------------------------------------------

  -- Begin exception handling routine. Modify KU, IE internally, and signal
  -- pipeline flush.
  start_exception <=
    '1' when (any_exception or (Status_IEc and any_interrupt)) = '1' else '0';

  -- Asserted if at least one interrupt is pending and enabled (unmasked)
  any_interrupt <= '1' when (Cause_IP and Status_IM) /= X"00" else '0';

  -- Asserted if at least one exception is asserted
  any_exception <= Mod_exception    or
                   TLBL_exception   or
                   TLBS_exception   or
                   AdEL_exception   or
                   AdES_exception   or
                   IBE_exception    or
                   DBE_exception    or
                   SYS_exception    or
                   BRK_exception    or
                   RI_exception     or
                   CpU_exc_internal or
                   Ov_Exception;    
  
  -- Asserted if at least one exception is asserted
  -- any_exception <= '1' when exception_slv /= (others => '0') else '0';
  --
  -- More convenient form for various exception signals
  --exception_slv <= Mod_exception &
  --                 TLBL_exception &
  --                 TLBS_exception &
  --                 AdEL_exception &
  --                 AdES_exception &
  --                 IBE_exception &
  --                 DBE_exception &
  --                 SYS_exception &
  --                 BRK_exception &
  --                 RI_exception &
  --                 CpU_exc_internal &
  --                 Ov_Exception;
  

  
  -- This should probably a *non-priority* encoder, because none of these
  -- exceptions should ever happen simultaneously.
  ExcCode_next <=
    Int_ExcCode       when any_interrupt = '1' else
    Mod_ExcCode       when mod_exception = '1' else
    TLBL_ExcCode      when TLBL_exception = '1' else
    TLBS_ExcCode      when TLBS_exception = '1' else
    AdEL_ExcCode      when AdEL_exception = '1' else
    AdES_ExcCode      when AdES_exception = '1' else
    IBE_ExcCode       when IBE_exception = '1' else
    DBE_ExcCode       when DBE_exception = '1' else
    SYS_ExcCode       when SYS_exception = '1' else
    BRK_ExcCode       when BRK_Exception = '1' else
    RI_ExcCode        when RI_Exception = '1' else
    CpU_ExcCode       when CpU_Exc_internal = '1' else
    Ov_ExcCode;     -- when Ov_ExcCode = '1';

  
  -- Sent to program counter to load exception vector address
  exc_vector_mux <=
    EXC_TLB_MISS        when (TLBL_exception or TLBS_exception) = '1' else
    EXC_GENERAL;
  -- Should we use EXC_RESET here?

  -- Illegal user mode access attempt
  CpU_exc_internal <= 
    '1' when CpU_exception = '1' else
    '1' when (cp0_wr_en and (Status_KUc nor Status_CU0)) = '1' else
    '0';

  
  ----------------------------------------------------------------------------  
  -- CP0 registers
  ----------------------------------------------------------------------------

  -- Must be in kernel mode, or set Status_CU0 to allow user CP0 access
  do_cp0_write <=
    '1' when (cp0_wr_en and (Status_KUc or Status_CU0)) = '1' else '0';

  compare_match <= '1' when Count_reg = Compare_reg else '0';

    
  -- Precedence is critical: reset > start_exception > do_rfe > user write
  Status_write_proc:
  process (clk)
  begin
    if rising_edge(clk) then
      
      if reset = '1' then
        -- Boot in Kernel mode with interrupts disabled, user locked out of CP0,
        -- default endianness, boot exception vectors.
        Status_KUc <= '1';
        Status_IEc <= '0';
        Status_CU0 <= '0';
        Status_RE  <= '0';
        Status_BEV <= '1';
        Status_IM <= (others => '0');
      elsif start_exception = '1' then
        -- Push KU/IE stack
        Status_KUc <= '1';
        Status_KUp <= Status_KUc;
        Status_KUo <= Status_KUp;
        Status_IEc <= '0';
        Status_IEp <= Status_IEc;
        Status_IEo <= Status_IEp;        
      elsif cp0_do_rfe = '1' then
        -- Pop KU/IE stack
        Status_KUc <= Status_KUp;
        Status_KUp <= Status_KUo;
        Status_IEc <= Status_IEp;
        Status_IEp <= Status_IEo;
      elsif (Status_reg_sel and do_cp0_write) = '1' then
        Status_reg <= cp0_wr_data;
      end if;
    end if;

    -- Ultimately these bits *should* get optimized out, but I'm temporarily
    -- disabling that to simplify debugging:
    
    -- Avoid inferring FF's for these bits by unconditionally zeroing them:
    -- Bitfields that are valid, but always zero in this implementation
    --Status_CU3 <= '0';
    --Status_CU2 <= '0';
    --Status_CU1 <= '0';      -- Zero because these coprocessors don't exist.
    
    --Status_TS <= '0';       -- Zero because we don't have a TLB.
    --Status_PE <= '0';       
    --Status_CM <= '0';
    --Status_PZ <= '0';
    --Status_SwC <= '0';
    --Status_IsC <= '0';      -- Zero because we don't have caches.
    
    -- Bitfields that are not defined by the ISA (zero for any implementation)
    --Status_reg(27 downto 26) <= "00";
    --Status_reg(24 downto 23) <= "00";
    --Status_reg(7 downto 6) <= "00";
  end process;

  
  Cause_write_proc:
  process (clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        -- Boot with no pending interrupts. Other bits should be ignored
        -- until an exception has happened.
        Cause_IP <= (others => '0');
      else
        -- User write may be overwritten by hardware.
        if (Cause_reg_sel and do_cp0_write) = '1' then
          Cause_reg <= cp0_wr_data;
        end if;
        
        if start_exception = '1' then
          Cause_BD <= cp0_BD_in;
          Cause_ExcCode <= std_logic_vector(ExcCode_next);
        end if;

        -- Poll hardware interrupt lines, leave software interrupt bits alone.
        Cause_IP(7 downto 3) <= hw_ext_interrupts(5 downto 1);

        -- Compare match is hardwired to hw_interrupt_0
        Cause_IP(2) <= compare_match;
      end if;
    end if;

    -- Ultimately these bits *should* get optimized out, but I'm temporarily
    -- disabling that to simplify debugging:
    
    -- Avoid inferring FF's for these bits by unconditionally zeroing them:
    -- Bitfields that are not defined by the ISA (zero for any implementation)
    --Cause_reg(30) <= '0';
    --Cause_reg(27 downto 16) <= (others => '0');
    --Cause_reg(7) <= '0';
    --Cause_reg(1 downto 0) <= "00";
  end process;

  
  EPC_write_proc:
  process (clk)
  begin
    if rising_edge(clk) then
      if start_exception = '1' then
        EPC_reg <= cp0_EPC_in;
      elsif (EPC_reg_sel and do_cp0_write) = '1' then
        EPC_reg <= cp0_wr_data;
      end if;
    end if;
  end process;

  
  BadVaddr_write_proc:
  process (clk)
  begin
    if rising_edge(clk) then
      if start_exception = '1' then
        BadVaddr_reg <= cp0_BadVaddr_in;
      elsif (BadVaddr_reg_sel and do_cp0_write) = '1' then
        BadVaddr_reg <= cp0_wr_data;
      end if;      
    end if;
  end process;


  Count_write_proc:
  process (clk)
  begin
    if rising_edge (clk) then
      if reset = '1' then
        Count_reg <= (others => '0');
      elsif (Count_reg_sel and do_cp0_write) = '1' then
        Count_reg <= unsigned(cp0_wr_data(23 downto 0));
      elsif Count_reg = Compare_reg then
        Count_reg <= (others => '0');
      else
        Count_reg <= Count_reg + 1;
      end if;
    end if;
  end process;

  
  Compare_write_proc:
  process (clk)
  begin
    if rising_edge (clk) then
      if reset = '1' then
        Compare_reg <= (others => '1');
      elsif (Compare_reg_sel and do_cp0_write) = '1' then
        Compare_reg <= unsigned(cp0_wr_data(23 downto 0));
      end if;
    end if;
  end process;
  
  
  
  -- No enforcement of illegal CP0 numbers yet!
  -- ALSO DOES READ assignment - consider renaming!
  cp0_reg_sel_proc:
  process (cp0_reg_num,
           EPC_reg, Cause_reg, Status_reg, BadVaddr_reg,
           Compare_reg, Count_reg)
  begin

    Status_reg_sel <= '0';
    Cause_reg_sel <= '0';
    EPC_reg_sel <= '0';
    BadVaddr_reg_sel <= '0';
    Compare_reg_sel <= '0';
    Count_reg_sel <= '0';

    -- Safe default
    cp0_rd_data <= PRId_constant;
    
    case to_integer(cp0_reg_num) is
      
      when 15 => -- PRId 
        cp0_rd_data <= PRId_constant;
        
      when 14 => -- EPC
        cp0_rd_data <= EPC_reg;
        EPC_reg_sel <= '1';
        
      when 13 => -- Cause
        cp0_rd_data <= Cause_reg;
        Cause_reg_sel <= '1';
        
      when 12 => -- SR            
        cp0_rd_data <= Status_reg;
        Status_reg_sel <= '1';
        
      when 11 => -- Compare
        cp0_rd_data <= X"00" & std_logic_vector(Compare_reg);
        Compare_reg_sel <= '1';
        
      when 9 =>  -- Count
        cp0_rd_data <= X"00" & std_logic_vector(Count_reg);
        Count_reg_sel <= '1';
        
      when 8 =>  -- BadVaddr
        cp0_rd_data <= BadVaddr_reg;
        BadVaddr_reg_sel <= '1';
        
      when others =>
        -- do nothing
        
    end case;
  end process;
  
end arch;
