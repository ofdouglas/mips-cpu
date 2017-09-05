
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity divider_test is
end divider_test;

architecture Behavioral of divider_test is

  -- N-bit integer division
  constant N : integer := 4;
  
  signal valid       : std_logic;
  signal ready       : std_logic;
  signal done        : std_logic;
  signal is_unsigned : std_logic;
  signal numerator   : std_logic_vector(N-1 downto 0);
  signal denominator : std_logic_vector(N-1 downto 0);
  signal quotient    : std_logic_vector(N-1 downto 0);
  signal remainder   : std_logic_vector(N-1 downto 0);

  signal clk : std_logic;
  constant T_CLK : time := 1 ns;

begin
  
  uut: entity work.signed_divider
    generic map (N => N)
    port map    (clk => clk,
                 valid => valid,
                 ready => ready,
                 done => done,
                 is_unsigned => is_unsigned,
                 numerator => numerator,
                 denominator => denominator,
                 quotient => quotient,
                 remainder => remainder);

  
  clk_proc: process
  begin
    clk <= '0';
    wait for T_CLK/2;
    clk <= '1';
    wait for T_CLK/2;
  end process;

  
  stim_proc: process
    variable quot_expected, remn_expected : integer;
    variable quot_result, remn_result     : integer;
  begin

    -- Unsigned Division Test
    is_unsigned <= '1';
    for numr in 1 to 2**N-1 loop
      for denm in 1 to 2**N-1 loop
        
        numerator <= std_logic_vector(to_unsigned(numr, N));
        denominator <= std_logic_vector(to_unsigned(denm, N));
        
        quot_expected := numr / denm;
        remn_expected := numr - denm * quot_expected;
        
        wait until ready = '1';
        valid <= '1';
        wait until done = '1';
        valid <= '0';

        quot_result := to_integer(unsigned(quotient));
        remn_result := to_integer(unsigned(remainder));
        
        assert (quot_expected = quot_result and
                remn_expected = remn_result)
          report integer'image(numr) & "/" & integer'image(denm)
          & " = " & integer'image(quot_result)
          & " R " & integer'image(remn_result)
          severity failure;

        wait for T_CLK;
      end loop;
    end loop;

    
    -- Signed Division Test
    is_unsigned <= '0';
    for numr in -(2**(N-1)-1) to 2**(N-1) -1 loop
      for denm in -(2**(N-1)-1) to 2**(N-1) -1 loop
        if denm = 0 then 
          next; -- skip undefined results
        end if;

        numerator <= std_logic_vector(to_signed(numr, N));
        denominator <= std_logic_vector(to_signed(denm, N));
        
        quot_expected := numr / denm;
        remn_expected := numr - denm * quot_expected;

        wait until ready = '1';
        valid <= '1';
        wait until done = '1';
        valid <= '0';
        
        quot_result := to_integer(signed(quotient));
        remn_result := to_integer(signed(remainder));
         
        assert (quot_expected = quot_result and
                remn_expected = remn_result)
          report integer'image(numr) & "/" & integer'image(denm)
          & " = " & integer'image(quot_result)
          & " R " & integer'image(remn_result)
          severity failure;

        wait for T_CLK;
      end loop;
    end loop;


    
    assert(0 = 1) report "Simulation passed" severity failure;
  end process;


end Behavioral;
