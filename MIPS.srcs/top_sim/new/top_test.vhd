library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.MIPS_r3000_ISA.ALL;

entity top_test is
end top_test;

architecture arch of top_test is

  constant T_CLK : time := 10 ns;
  
  signal clk : std_logic;
  signal leds : std_logic_vector(15 downto 0);
  signal CPU_RESETN : std_logic;
  
begin

  process
  begin
    clk <= '0';
    wait for T_CLK / 2;
    clk <= '1';
    wait for T_CLK / 2;
  end process;

  CPU_RESETN <= '1';
  
  --process
  --begin
  --  CPU_RESETN <= '1';
  --  wait for T_CLK * 8;
  --  CPU_RESETN <= '0';
  --  wait for T_CLK;
  --  CPU_RESETN <= '1';
  --  wait;
  --end process;
  

  uut: entity work.top
    port map(clk => clk,
             CPU_RESETN => CPU_RESETN,
             leds => leds);
  

end arch;
