library IEEE, STD;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use STD.textio.all;
use IEEE.std_logic_textio.all;


use WORK.MIPS_r3000_ISA.ALL;


entity barrel_shifter_test is
end barrel_shifter_test;

architecture Behavioral of barrel_shifter_test is
  
  signal shift_input : std_logic_vector(31 downto 0);
  signal shift_amount : unsigned(4 downto 0);
  signal shift_code : std_logic_vector(1 downto 0);
  signal shift_result : std_logic_vector(31 downto 0);

  type test_record is record
    -- Test inputs
    shift_input  : std_logic_vector(31 downto 0);
    shift_amount : unsigned(4 downto 0);
    shift_code   : std_logic_vector(1 downto 0);

    -- Test result
    shift_result : std_logic_vector(31 downto 0);
  end record;

  constant NUM_TESTS : integer := 3 * 10;
  
  type test_vector is array(1 to NUM_TESTS) of test_record;

  constant test_inputs : test_vector := (
    (X"0000_0000", "00000", SLL_CODE, X"0000_0000"),
    (X"0000_0000", "00000", SRL_CODE, X"0000_0000"),
    (X"0000_0000", "00000", SRA_CODE, X"0000_0000"),
    
    (X"0000_0000", "11111", SLL_CODE, X"0000_0000"),
    (X"0000_0000", "11111", SRL_CODE, X"0000_0000"),
    (X"0000_0000", "11111", SRA_CODE, X"0000_0000"),
    
    (X"ffff_ffff", "00000", SLL_CODE, X"ffff_ffff"),
    (X"ffff_ffff", "00000", SRL_CODE, X"ffff_ffff"),
    (X"ffff_ffff", "00000", SRA_CODE, X"ffff_ffff"),
    
    (X"ffff_ffff", "11111", SLL_CODE, X"8000_0000"),
    (X"ffff_ffff", "11111", SRL_CODE, X"0000_0001"),
    (X"ffff_ffff", "11111", SRA_CODE, X"ffff_ffff"),

    (X"0000_0003", "00001", SLL_CODE, X"0000_0006"),
    (X"0000_0003", "00001", SRL_CODE, X"0000_0001"),
    (X"0000_0003", "00001", SRA_CODE, X"0000_0001"),

    (X"c000_0000", "00001", SLL_CODE, X"8000_0000"),
    (X"c000_0000", "00001", SRL_CODE, X"6000_0000"),
    (X"c000_0000", "00001", SRA_CODE, X"e000_0000"), 
    
    (X"849f_d23e", "10011", SLL_CODE, X"91f0_0000"),
    (X"849f_d23e", "10011", SRL_CODE, X"0000_1093"),
    (X"849f_d23e", "10011", SRA_CODE, X"ffff_f093"),

    (X"f012_7bb3", "01100", SLL_CODE, X"27bb_3000"),
    (X"f012_7bb3", "01100", SRL_CODE, X"000f_0127"),
    (X"f012_7bb3", "01100", SRA_CODE, X"ffff_0127"), 

    (X"561c_75b9", "01101", SLL_CODE, X"8eb7_2000"),
    (X"561c_75b9", "01101", SRL_CODE, X"0002_b0e3"),
    (X"561c_75b9", "01101", SRA_CODE, X"0002_b0e3"), 

    (X"e1ff_9fa2", "01000", SLL_CODE, X"f9fa_2000"),
    (X"e1ff_9fa2", "01000", SRL_CODE, X"00e1_ff9f"),
    (X"e1ff_9fa2", "01000", SRA_CODE, X"ffe1_ff9f"),

    (X"e1ff_9fa2", "00101", SLL_CODE, X"3ff3_f440"),
    (X"e1ff_9fa2", "00101", SRL_CODE, X"070f_fcfd"),
    (X"e1ff_9fa2", "00101", SRA_CODE, X"f70f_fcfd")
    );

-- template
--  (X"0000_0000", "00000", SLL_CODE, X"0000_0000"),
--  (X"0000_0000", "00000", SRL_CODE, X"0000_0000"),
--  (X"0000_0000", "00000", SRA_CODE, X"0000_0000"), 



  
  
begin

  uut: entity work.barrel_shifter
    port map (shift_input => shift_input,
              shift_amount => shift_amount,
              shift_code => shift_code,
              shift_result => shift_result);

  stim_proc: process
    variable my_line : line;
  begin
    for i in 1 to NUM_TESTS loop
      wait for 1 ns;
      shift_input <= test_inputs(i).shift_input;
      shift_amount <= test_inputs(i).shift_amount;
      shift_code <= test_inputs(i).shift_code;
      
      wait for 1 ns;
      if shift_result /= test_inputs(i).shift_result then
        write(my_line, string'("ERROR: "));
        case shift_code is
          when SLL_CODE => write(my_line, string'("SLL "));
          when SRL_CODE => write(my_line, string'("SRL "));
          when SRA_CODE => write(my_line, string'("SRA "));
          when others =>
        end case;
        write(my_line, std_logic_vector(shift_amount));
        
        write(my_line, LF & string'("Input:  "));
        hwrite(my_line, shift_input);
        
        write(my_line, LF & string'("Output: "));
        hwrite(my_line, shift_result);
        
        writeline(OUTPUT, my_line);
        
        assert 0 = 1 severity failure;
      end if;
    end loop;

    assert 0 = 2 report "SIMULATION PASSED" severity failure;
  end process;
        

end Behavioral;
