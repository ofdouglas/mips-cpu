library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.MIPS_r3000_ISA.ALL;

entity cpu_sim is
end cpu_sim;


architecture foo of cpu_sim is

  constant T_CLK : time := 1 ns;
  
  signal clk : std_logic;
  signal reset : std_logic;
  signal hw_ext_interrupts : std_logic_vector(5 downto 0);

  signal ibus_addr : unsigned(31 downto 0);
  signal ibus_rd_data : std_logic_vector(31 downto 0);
  signal ibus_rd_en : std_logic;
  signal ibus_fault : std_logic;
  
  signal dbus_lane_en : std_logic_vector(3 downto 0);
  signal dbus_rd_en : std_logic;
  signal dbus_wr_en : std_logic;
  
  signal dbus_addr : unsigned(31 downto 0);
  signal dbus_rd_data : std_logic_vector(31 downto 0);
  signal dbus_wr_data : std_logic_vector(31 downto 0);
  signal dbus_fault : std_logic;
  
begin
  
  uut: entity work.r3000_cpu
    port map(clk => clk,
             reset => reset,
             hw_ext_interrupts => hw_ext_interrupts,

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
  

  clk_proc:
  process
  begin
    wait for T_CLK / 2;
    clk <= '1';
    wait for T_CLK / 2;
    clk <= '0';
  end process;

  reset <= '0';
  hw_ext_interrupts <= (others => '0');


  

end foo;
