
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

-- Simple enough that it doesn't really need it's own file, but this helps
-- prevent it from cluttering the r3000_cpu.vhd level of the hierarchy


entity jump_unit is
  port (gen_opcode        : in std_logic_vector(3 downto 0);
        jump_pc4_top      : in unsigned(31 downto 28);
        jump_address      : in unsigned(25 downto 0);
        jump_rs_reg_data  : in unsigned(31 downto 0);
        jump_result       : out unsigned(31 downto 0);
        jump_align_error  : out std_logic
        );
end jump_unit;

architecture arch of jump_unit is

  alias jump_opcode : std_logic is gen_opcode(0);
  
begin

  jump_result <=
    jump_pc4_top & jump_address & "00"    when jump_opcode = J_CODE else
    jump_rs_reg_data;                  -- when jump_opcode = JR_CODE;

  jump_align_error <=
    '1' when jump_opcode = JR_CODE and jump_rs_reg_data(1 downto 0) /= "00" else
    '0';
  
end arch;
