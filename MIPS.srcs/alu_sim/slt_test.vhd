
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

entity slt_test is
end slt_test;

architecture Behavioral of slt_test is

  -- N-bit ALU
  constant N : integer := 4;
  
  signal alu_opcode   : std_logic_vector(3 downto 0);
  signal alu_input_A  : std_logic_vector(N-1 downto 0);
  signal alu_input_B  : std_logic_vector(N-1 downto 0);
  signal alu_result   : std_logic_vector(N-1 downto 0);
  signal alu_overflow : std_logic;

begin
  
  uut: entity work.alu
    generic map (N => N)
    port map    (alu_opcode => alu_opcode,
                 alu_input_A => alu_input_A,
                 alu_input_B => alu_input_B,
                 alu_result => alu_result,
                 alu_overflow => alu_overflow
                 );
  
  stim_proc: process
    variable less_than : std_logic;
  begin

    -- SLTU test
    alu_opcode <= SLTU_CODE;
    for i in 0 to (2**N) -1 loop
      for j in 0 to (2**N) -1 loop

        alu_input_A <= std_logic_vector(to_unsigned(i, N));
        alu_input_B <= std_logic_vector(to_unsigned(j, N));
        wait for 1 ps;

        fucking fuck you
          
        if to_unsigned(i, N+1) < to_unsigned(j, N+1) then
          less_than := '1';
        else
          less_than := '0';
        end if;

        assert (alu_result(0) = less_than)
          report integer'image(i) & " < " & integer'image(j) & " = " 
          & std_logic'image(less_than)
          severity failure;
        
      end loop;
    end loop;

    -- SLTU test
    alu_opcode <= SLTU_CODE;
    for i in -(2**(N-1)-1) to 2**(N-1) -1 loop
      for j in -(2**(N-1)-1) to 2**(N-1) -1 loop

        alu_input_A <= std_logic_vector(to_signed(i, N));
        alu_input_B <= std_logic_vector(to_signed(j, N));
        wait for 1 ps;

        if i < j then
          less_than := '1';
        else
          less_than := '0';
        end if;

        assert (alu_result(0) = less_than)
          report integer'image(i) & " < " & integer'image(j) & " = " 
          & std_logic'image(less_than)
          severity failure;
        
      end loop;
    end loop;

    assert(0 = 1) report "Simulation passed" severity failure;
  end process;


end Behavioral;
