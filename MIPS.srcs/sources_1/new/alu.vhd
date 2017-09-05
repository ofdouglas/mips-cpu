
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;
  
entity alu is
  generic (N : integer := 32);
  port (alu_opcode   : in std_logic_vector(3 downto 0);
        alu_input_A  : in std_logic_vector(N-1 downto 0);
        alu_input_B  : in std_logic_vector(N-1 downto 0);
        alu_result   : out std_logic_vector(N-1 downto 0);
        alu_overflow : out std_logic);
end alu;

architecture Behavioral of alu is

  -- extra msb to access carry out
  signal temp_A      : unsigned(N downto 0);
  signal temp_B      : unsigned(N downto 0);
  signal temp_result : unsigned(N downto 0);

  -- condition codes used internally
  signal less_than      : std_logic;

  -- Because Vivado simulator won't show us variables
  signal overflow_sim_signal : std_logic;
  
begin

  -- do 33-bit add/sub to provide access to carry out
  temp_A <= unsigned('0' & alu_input_A);
  temp_B <= unsigned('0' & alu_input_B);
  
  -- ALU function mux
  with alu_opcode select temp_result <=
    temp_A and temp_B     when AND_CODE,
    temp_A or  temp_B     when OR_CODE,    
    temp_A nor temp_B     when NOR_CODE,
    temp_A xor temp_B     when XOR_CODE,
    temp_A  -  temp_B     when SUBU_CODE,    
    temp_A  -  temp_B     when SUB_CODE,
    temp_A  -  temp_B     when SLT_CODE,
    temp_A  -  temp_B     when SLTU_CODE,
    temp_A  +  temp_B     when ADDU_CODE,
    temp_A  +  temp_B     when ADD_CODE,
    temp_A  +  temp_B     when others;

  -- ALU result mux
  alu_result <= (N-1 downto 1 => '0') & less_than
                when alu_opcode = SLT_CODE or alu_opcode = SLTU_CODE else
                std_logic_vector(temp_result(N-1 downto 0));

  
  condition_code_logic:
  process (alu_opcode, temp_A, temp_B, temp_result)
    variable overflow_bit : std_logic;
  begin

    -- Arithmetic overflow, reused by SLT code:
    -- Addition: MSBs were the same before, but the result has a different MSB
    -- Sub/SLT:  Same, but B is negated to do subtraction, turning xnor into xor
    overflow_bit := '0';
    if alu_opcode = ADD_CODE then
      overflow_bit := (temp_A(N-1) xnor temp_B(N-1)) and
                      (temp_result(N-1) xor temp_A(N-1));
    else
      overflow_bit := (temp_A(N-1) xor temp_B(N-1)) and
                      (temp_result(N-1) xor temp_A(N-1));
    end if;

    overflow_sim_signal <= overflow_bit;

    -- Actual *overflow exception*
    if alu_opcode = ADD_CODE or alu_opcode = SUB_CODE then    
      alu_overflow <= overflow_bit;
    else
      alu_overflow <= '0';
      
    end if;

    -- Set less than logic.
    -- Since (a < b) iff (a - b < 0), check the sign of the result. For SLTU,
    -- The Nth bit makes temp_result a valid N+1 bit signed int with no overflow.
    -- For SLT, the zero extension would be a problem, so use the MSB of the N-1
    -- -bit result and invert the result if the sign changed due to overflow.

    if alu_opcode = SLT_CODE then
      less_than <= temp_result(N-1) xor overflow_bit;
    elsif alu_opcode = SLTU_CODE then
      less_than <= temp_result(N);
    else
      less_than <= '0';
    end if;
    
  end process;
  
end Behavioral;

