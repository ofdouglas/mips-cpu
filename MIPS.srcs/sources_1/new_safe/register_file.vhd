-- MIPS Register File:
-- 2 read ports, 1 write port. 31 GPRs, plus r0 (constant 0)

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity register_file is
  port (clk            : in std_logic;
        -- Read ports
        rgf_rs_num     : in unsigned(4 downto 0);
        rgf_rt_num     : in unsigned(4 downto 0);
        rgf_rs_data    : out std_logic_vector(31 downto 0);
        rgf_rt_data    : out std_logic_vector(31 downto 0);
        -- Write Port
        rgf_write_num  : in unsigned(4 downto 0);
        rgf_write_data : in std_logic_vector(31 downto 0);
        rgf_write_en   : in std_logic
        );
end register_file;

architecture arch of register_file is

  -- Creating 31 registers (explicitly or by forcing regfile(0) to zero)
  -- results in Vivado implementing the entire regfile in discrete flip flops.
  --
  -- To use the Distributed RAM, a 32x32 register file is created, but the
  -- write enable is gated with an address comparator on the write address,
  -- so that the initial contents (zero) of regfile(0) cannot be overwritten.
  
  type regfile_t is array (31 downto 0) of std_logic_vector(31 downto 0);
  signal regfile : regfile_t := (others => (others => '0'));
  
begin

  -- Register File write port
  regfile_write_proc:
  process (clk)
    variable write_num : integer range 0 to 31;
  begin
    if rising_edge(clk) then
      write_num := to_integer(rgf_write_num);
      if rgf_write_en = '1' and write_num /= 0 then
        regfile(write_num) <= rgf_write_data;
      end if;
    end if;
  end process;

  -- Register File read ports
  rgf_rs_data <= regfile(to_integer(rgf_rs_num));
  rgf_rt_data <= regfile(to_integer(rgf_rt_num));
  
    

  
end arch;

