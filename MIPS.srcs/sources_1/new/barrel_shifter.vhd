
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

entity barrel_shifter is
  port    (shift_input  : in std_logic_vector(WORD_SIZE-1 downto 0);
           shift_amount : in unsigned(WORD_LOG2-1 downto 0);
           gen_opcode   : in std_logic_vector(3 downto 0);
           shift_result : out std_logic_vector(WORD_SIZE-1 downto 0));
end barrel_shifter;


architecture arch of barrel_shifter is

  alias shift_opcode : std_logic_vector(1 downto 0) is gen_opcode(1 downto 0);
  
  -- Group the shift_input with the WORD_LOG2 stages of actual shifting to
  -- simplify the indexing of stages in the for loops below.
  type stage_array is array (WORD_LOG2 downto 0)
    of std_logic_vector(WORD_SIZE-1 downto 0);
  
begin

  -- For a 32-bit barrel shifter:
  -- Decompose the shift amount into separate shifts of 16, 8, 4, 2, and 1. 
  -- Spread these out over 5 distinct stages to avoid synthesizing huge muxes.
  
  shift_proc:
  process (shift_input, shift_amount, shift_opcode)
    variable shamt     : integer;      -- Amount to shift in each stage
    variable tmp_index : integer;      -- Test index (may go out of bounds)
    variable direction : integer;      -- -1 to shift left, +1 to shift right
    variable new_bit   : std_logic;    -- MSB of shift_input for SRA, else 0
    variable stages    : stage_array;  -- Multi-stage barrel shifter
  begin
    -- Setup stages array
    stages(stages'high) := shift_input;
    
    -- Determine shift parameters
    direction := -1;
    new_bit := '0';
    if shift_opcode = SRL_CODE then
      direction := 1;
    elsif shift_opcode = SRA_CODE then
      direction := 1;
      new_bit := shift_input(shift_input'high);
    end if;

    -- Foreach shift stage (foreach power of 2 in shamt):
    for s in WORD_LOG2-1 downto 0 loop
      shamt := 2 ** s;

      -- Copy previous stage without shifting
      if shift_amount(s) = '0' then
        stages(s) := stages(s+1);
      else
        -- Foreach bit in stages(s):
        -- Shift previous stage by shamt, using new_bit when out of bounds
        
        for i in WORD_SIZE-1 downto 0 loop
          tmp_index := i + shamt * direction;
          if tmp_index >= 0 and tmp_index <= WORD_SIZE-1 then
            stages(s)(i) := stages(s+1)(tmp_index);
          else
            stages(s)(i) := new_bit;
          end if;

        end loop;
      end if;
      
    end loop;
    shift_result <= stages(stages'low);
  end process;
  
end arch;

