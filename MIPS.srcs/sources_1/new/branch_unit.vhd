library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

entity branch_unit is
  port(
    -- Control inputs
    gen_opcode         : in std_logic_vector(3 downto 0);
    
    -- Datapath inputs
    branch_pc_plus_4   : in unsigned(31 downto 0);
    branch_rs_reg_data : in unsigned(31 downto 0);
    branch_rt_reg_data : in unsigned(31 downto 0);
    branch_immediate   : in unsigned(15 downto 0);

    -- Datapath outputs
    branch_pc_result   : out unsigned(31 downto 0)
    );
end branch_unit;

architecture arch of branch_unit is

  alias branch_opcode : std_logic_vector(2 downto 0) is gen_opcode(2 downto 0);

  -- Branch decision logic
  signal eq_result    : std_logic;
  signal branch_taken : std_logic;
  
begin

  
  eq_input_proc:
  process (branch_rs_reg_data, branch_rt_reg_data, branch_opcode)
  begin
    eq_result <= '0';
    if branch_opcode = BEQ_CODE or branch_opcode = BNE_CODE then
      if branch_rs_reg_data = branch_rt_reg_data then
        eq_result <= '1';
      end if;
    else
      if branch_rs_reg_data(30 downto 0) = (30 downto 0 => '0') then
        eq_result <= '1';
      end if;
    end if;
  end process;

  
  -- Make branch decision using 'eq_result' and MSB of 'rs_reg_data'.
  branch_taken_proc:
  process (branch_opcode, eq_result, branch_rs_reg_data)
  begin

    -- Default case
    branch_taken <= '0';

    -- Branch Decision
    case branch_opcode is
      when BGTZ_CODE =>
        if branch_rs_reg_data(31) = '0' and eq_result = '0' then
          branch_taken <= '1';
        end if;
        
      when BLEZ_CODE =>
        if branch_rs_reg_data(31) = '1' or eq_result = '1' then
          branch_taken <= '1';
        end if;

      when BNE_CODE =>
        if eq_result = '0' then
          branch_taken <= '1';
        end if;

      when BEQ_CODE =>
        if eq_result = '1' then
          branch_taken <= '1';
        end if;

      when BGEZ_CODE =>
        if branch_rs_reg_data(31) = '0' and eq_result = '1' then
          branch_taken <= '1';
        end if;
        
      when BLTZ_CODE =>
        if branch_rs_reg_data(31) = '1' and eq_result = '0' then
          branch_taken <= '1';
        end if;

      when others =>
        -- not a branch instruction: result is ignored (don't care)
        
    end case;
  end process;


  branch_adder_proc:
  process (branch_taken, branch_pc_plus_4, branch_immediate)
    variable se_immediate : unsigned(31 downto 0);
  begin

    se_immediate := (31 downto 18 => branch_immediate(15)) & branch_immediate & "00";
    
    if branch_taken = '1' then
      branch_pc_result <= branch_pc_plus_4 + se_immediate;
    else
      branch_pc_result <= branch_pc_plus_4;
    end if;

  end process;

  
end arch;
