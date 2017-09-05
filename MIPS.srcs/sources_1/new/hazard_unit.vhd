
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

entity hazard_unit is
  port (EX_MEM_do_load       : in std_logic;
        EX_MEM_do_cp0_rd     : in std_logic;
        EX_MEM_reg_dest_num  : in unsigned(4 downto 0);
        EX_do_rs_read        : in std_logic;
        EX_do_rt_read        : in std_logic;
        ID_EX_rs_num         : in unsigned(4 downto 0);
        ID_EX_rt_num         : in unsigned(4 downto 0);
        ID_rs_num            : in unsigned(4 downto 0);
        ID_rt_num            : in unsigned(4 downto 0);
        ID_EX_do_reg_write   : in std_logic;
        ID_EX_reg_dest_num   : in unsigned(4 downto 0);
        ID_do_jump_branch    : in std_logic;
        ID_rs_reg_num        : in unsigned(4 downto 0);
        ID_do_rs_read        : in std_logic;
        ID_rt_reg_num        : in unsigned(4 downto 0);
        ID_do_rt_read        : in std_logic;
        
        load_use_stall       : out std_logic;
        jump_branch_stall    : out std_logic
        );
end hazard_unit;

architecture arch of hazard_unit is

begin
  
-- NOTE: we actually should not stall when 'needed' register is r0!
  load_use_stall_proc:
  process (EX_MEM_do_load, EX_MEM_reg_dest_num, EX_do_rs_read, EX_do_rt_read,
           EX_MEM_do_cp0_rd, ID_EX_rs_num, ID_EX_rt_num, ID_rs_num, ID_rt_num,
           ID_do_rs_read, ID_do_rt_read)
  begin
    load_use_stall <= '0';

    if EX_MEM_do_load = '1' or EX_MEM_do_cp0_rd = '1' then
      -- ID Stage
      if (ID_do_rs_read = '1' and EX_MEM_reg_dest_num = ID_rs_num) or
        (ID_do_rt_read = '1' and EX_MEM_reg_dest_num = ID_rt_num) then
        load_use_stall <= '1';
      end if;
      
      -- EX Stage
      if (EX_do_rs_read = '1' and EX_MEM_reg_dest_num = ID_EX_rs_num) or
        (EX_do_rt_read = '1' and EX_MEM_reg_dest_num = ID_EX_rt_num) then
        load_use_stall <= '1';
      end if;
    end if;
  end process;


  jump_branch_stall_proc:
  process (ID_EX_do_reg_write, ID_EX_reg_dest_num, ID_do_jump_branch,
           ID_rs_reg_num, ID_do_rs_read, ID_rt_reg_num, ID_do_rt_read)
  begin
    jump_branch_stall <= '0';
    
    if ID_EX_do_reg_write = '1' and ID_do_jump_branch = '1' then
      if ID_EX_reg_dest_num = ID_rs_reg_num and ID_do_rs_read = '1' then
        jump_branch_stall <= '1';
      elsif ID_EX_reg_dest_num = ID_rt_reg_num and ID_do_rt_read = '1' then
        jump_branch_stall <= '1';
      end if;
    end if;
    
  end process;

end arch;
