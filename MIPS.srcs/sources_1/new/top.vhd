
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- SEGMENT         VADDR                     PADDR
--  kuseg:   0x7fffffff:0x00000000         [user MMU]
--  kseg0:   0x9fffffff:0x80000000   0x1fffffff:0x00000000      cached
--  kseg1:   0xbfffffff:0xA0000000   0x1fffffff:0x00000000      uncached
--  kseg2:   0xffffffff:0xC0000000        [kernel MMU]
--
-- VECTORS:
-- cached:   0x80000100:0x80000080   0x00000100:0x00000080
-- uncached: 0xbfc00200:0xbfc00000   0x1fc00200:0x1fc00000      upper 4MB
-- *upper ends of vectors ranges are probably wrong...
--

-- segments     : out std_logic_vector(6 downto 0);
-- anodes       : out std_logic_vector(7 downto 0);
-- UART_TXD_IN  : out std_logic;
-- UART_RXD_OUT : in std_logic;



entity top is
  port (sys_clk      : in std_logic;
        CPU_RESETN   : in std_logic;    -- CPU_RESETN is not currently used.
        leds         : out std_logic_vector(15 downto 0)
        );
end top;

architecture arch of top is

  constant IMEM_DEPTH_LOG2 : integer := 14;
  constant DMEM_DEPTH_LOG2 : integer := 10;
  constant IMEM_DEPTH      : integer := 2**IMEM_DEPTH_LOG2;
  constant DMEM_DEPTH      : integer := 2**DMEM_DEPTH_LOG2;
  constant MEM_WIDTH       : integer := 32;
  constant IMEM_NUM_BYTES  : integer := IMEM_DEPTH * MEM_WIDTH / 8;
  constant DMEM_NUM_BYTES  : integer := DMEM_DEPTH * MEM_WIDTH / 8;
  constant IMEM_BASE_ADDR  : unsigned(31 downto 0) := X"bfc0_0000";
  constant DMEM_BASE_ADDR  : unsigned(31 downto 0)
    := IMEM_BASE_ADDR + IMEM_NUM_BYTES;
  constant LED_BASE_ADDR   : unsigned(31 downto 0) := X"bfc1_2000";

  constant NOP_INSTRUCTION : std_logic_vector(31 downto 0) := X"0000_0000";

  
  type instr_mem_type is array (0 to IMEM_DEPTH -1)
    of std_logic_vector(31 downto 0);
  
  type data_mem_type is array (0 to DMEM_DEPTH -1)
    of std_logic_vector(31 downto 0);

  -- Test program:
  signal instr_mem : instr_mem_type := (
    -- Blinky test
    ------------------------------------------------------
    X"10000007",    -- 0xbfc00000:  b	0xbfc00020
    X"00000000",    -- 0xbfc00004:  nop
    X"1000fffd",    -- 0xbfc00008:  b	0xbfc00000
    X"00000000",    -- 0xbfc0000c:  nop
    X"24630001",    -- 0xbfc00010:  addiu	v1,v1,1
    X"401a7000",    -- 0xbfc00014:  mfc0	k0,c0_epc
    X"03400008",    -- 0xbfc00018:  jr	k0
    X"42000010",    -- 0xbfc0001c:  c0	0x10
    X"3c010001",    -- 0xbfc00020:  lui	at,0x1
    X"342186a0",    -- 0xbfc00024:  ori	at,at,0x86a0
    X"40815800",    -- 0xbfc00028:  mtc0	at,$11
    X"40016000",    -- 0xbfc0002c:  mfc0	at,c0_sr
    X"34210410",    -- 0xbfc00030:  ori	at,at,0x410
    X"40816000",    -- 0xbfc00034:  mtc0	at,c0_sr
    X"24030000",    -- 0xbfc00038:  li	v1,0
    X"3c04bfc1",    -- 0xbfc0003c:  lui	a0,0xbfc1
    X"34842000",    -- 0xbfc00040:  ori	a0,a0,0x2000
    X"286501f4",    -- 0xbfc00044:  slti	a1,v1,500
    X"14a0fffe",    -- 0xbfc00048:  bnez	a1,0xbfc00044
    X"00000000",    -- 0xbfc0004c:  nop
    X"00001821",    -- 0xbfc00050:  move	v1,zero
    X"8c860000",    -- 0xbfc00054:  lw	a2,0(a0)
    X"38c60001",    -- 0xbfc00058:  xori	a2,a2,0x1
    X"ac860000",    -- 0xbfc0005c:  sw	a2,0(a0)
    X"0bf00011",    -- 0xbfc00060:  j	0xbfc00044
    X"00000000",    -- 0xbfc00064:  nop
    others => NOP_INSTRUCTION
    );

  -- Instruction Memory Bus
  signal ibus_addr    : unsigned(31 downto 0);
  signal ibus_rd_data : std_logic_vector(31 downto 0);
  signal ibus_rd_en   : std_logic;
  signal ibus_fault   : std_logic;

  -- Data memory Bus
  signal data_mem     : data_mem_type := (others => (others => '0'));
  signal dbus_addr    : unsigned(31 downto 0);
  signal dbus_lane_en : std_logic_vector(0 to 3);
  signal dbus_wr_data : std_logic_vector(31 downto 0);
  signal dbus_rd_data : std_logic_vector(31 downto 0);
  signal dbus_rd_en   : std_logic;
  signal dbus_wr_en   : std_logic;
  signal dbus_fault   : std_logic;

  
  -- Interrupts for devices like UART, etc (not used yet)
  signal hw_ext_interrupts : std_logic_vector(5 downto 0);

  -- LEDs on the board, which we want to blink
  signal leds_reg : std_logic_vector(15 downto 0) := (others => '0');

  -- For monitoring exception entry with an oscilloscope (not used currently)
  -- Should be asserted briefly when an exception begins.
  signal exception_out : std_logic;


  -- The CPU requires reset to be asserted for at least 4 clock cycles
  -- during boot, to place the pipeline in a deterministic state.
  -- Keep it in reset for 16 clocks just to be sure we aren't hitting bugs
  -- with GSR deassertion.
  signal reset_sync  : std_logic_vector(15 downto 0) := (others => '1');

  -- CPU clock derived from MMCM / PLL
  signal clk : std_logic;

  signal reset_lock : std_logic;
  
  component clk_wiz_0
    port(clk_in1 : in     std_logic;
         clk     : out    std_logic;
         locked  : out    std_logic);
  end component;

  
begin

clk_gen0 : clk_wiz_0
  port map (clk_in1 => sys_clk,
            clk => clk,
            locked => reset_lock);

  -- Debug outputs
  leds <= leds_reg;

  -- Currently unused
  hw_ext_interrupts <= (others => '0');

  -- Generate reset (active high) for CPU at boot
  reset_proc:
  process (clk)
  begin
    if rising_edge(clk) then
      if reset_lock = '1' then
      reset_sync <= '0' & reset_sync(15 downto 1);
      end if;
    end if;
  end process;
  
  
  imem_proc:
  process (clk, ibus_addr)
    variable addr : integer;
  begin
    addr := to_integer(ibus_addr(IMEM_DEPTH_LOG2 -1 downto 2));
    ibus_rd_data <= instr_mem(addr);

    if rising_edge(clk) then
      ibus_fault <= '0';
      if ibus_addr(31 downto IMEM_DEPTH_LOG2) /=
        IMEM_BASE_ADDR(31 downto IMEM_DEPTH_LOG2) then
        ibus_fault <= '1';
      end if;
    end if;
    
  end process;


  
  dmem_proc:
  process (clk, dbus_addr, dbus_lane_en, leds_reg)
    variable addr : integer;
  begin

    -- Bus Fault for invalid address
    -- Can't use this until we have a read enable signal!
    --      
    --if dbus_addr(31 downto DMEM_DEPTH_LOG2) /=
    --  DMEM_BASE_ADDR(31 downto DMEM_DEPTH_LOG2) then
    --  dbus_fault <= '1';
    --end if;

    dbus_fault <= '0';
    addr := to_integer(dbus_addr(DMEM_DEPTH_LOG2 -1 downto 2));

    dbus_rd_data <= X"0000" & leds_reg;
    if dbus_addr(31 downto DMEM_DEPTH_LOG2) =
      DMEM_BASE_ADDR(31 downto DMEM_DEPTH_LOG2) then
      dbus_rd_data <= data_mem(addr);
    end if;

    if rising_edge(clk) then
      
      if dbus_wr_en = '1' then
        if dbus_addr(31 downto DMEM_DEPTH_LOG2) =
          DMEM_BASE_ADDR(31 downto DMEM_DEPTH_LOG2) then
          -- Byte-wide Write Enable
          if dbus_lane_en(0) = '1' then
            data_mem(addr)(31 downto 24) <= dbus_wr_data(31 downto 24);
          end if;
          if dbus_lane_en(1) = '1' then
            data_mem(addr)(23 downto 16) <= dbus_wr_data(23 downto 16);
          end if;
          if dbus_lane_en(2) = '1' then
            data_mem(addr)(15 downto 8) <= dbus_wr_data(15 downto 8);
          end if;
          if dbus_lane_en(3) = '1' then
            data_mem(addr)(7 downto 0) <= dbus_wr_data(7 downto 0);
          end if;
        elsif dbus_addr = LED_BASE_ADDR then
          leds_reg <= dbus_wr_data(15 downto 0);
        end if;
        
      -- MIPS1 architecture doesn't accept bus fault on write, because
      -- the use of a write buffer would make the exception imprecise.
      end if;

    end if;
  end process;


  
  mips_cpu: entity work.r3000_cpu
    port map (clk => clk,
              reset => reset_sync(0),
              hw_ext_interrupts => hw_ext_interrupts,
              exception_out => exception_out,
              ibus_addr => ibus_addr,
              ibus_rd_data => ibus_rd_data,
              ibus_rd_en => ibus_rd_en,
              ibus_fault => ibus_fault,
              dbus_lane_en => dbus_lane_en,
              dbus_rd_en => dbus_rd_en,
              dbus_wr_en => dbus_wr_en,
              dbus_addr => dbus_addr,
              dbus_rd_data => dbus_rd_data,
              dbus_wr_data => dbus_wr_data,
              dbus_fault => dbus_fault
              );
  
end arch;
