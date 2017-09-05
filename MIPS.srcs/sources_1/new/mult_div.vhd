
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

entity mult_div is
  port (clk          : in std_logic;
        gen_opcode   : in std_logic_vector(3 downto 0);
        do_mdiv_op   : in std_logic;
        rs_reg_data  : in std_logic_vector(31 downto 0);
        rt_reg_data  : in std_logic_vector(31 downto 0);
        mdiv_result  : out std_logic_vector(31 downto 0);
        mdiv_stall   : out std_logic
        );
end mult_div;

architecture arch of mult_div is

  -- Divider ports
  signal div_valid       : std_logic;
  signal div_ready       : std_logic;
  signal div_done        : std_logic;
  signal div_is_unsigned : std_logic;
  signal div_numerator   : std_logic_vector(31 downto 0);
  signal div_denominator : std_logic_vector(31 downto 0);
  signal div_quotient    : std_logic_vector(31 downto 0);
  signal div_remainder   : std_logic_vector(31 downto 0);

  -- Multiplier ports
  signal mult_valid       : std_logic;
  signal mult_ready       : std_logic;
  signal mult_done        : std_logic;
  signal mult_is_unsigned  : std_logic;
  signal mult_multiplicand : std_logic_vector(31 downto 0);
  signal mult_multiplier   : std_logic_vector(31 downto 0);
  signal mult_result_hi    : std_logic_vector(31 downto 0);
  signal mult_result_lo    : std_logic_vector(31 downto 0);

  -- LO and HI registers
  signal lo_reg : std_logic_vector(31 downto 0);
  signal hi_reg : std_logic_vector(31 downto 0);

  -- Controls
  alias mdiv_opcode   : std_logic_vector(1 downto 0) is gen_opcode(1 downto 0);
  alias lo_hi_opcode  : std_logic_vector(1 downto 0) is gen_opcode(1 downto 0);
  signal do_mdiv  : std_logic;
  signal do_lo_hi : std_logic;
  
begin
  
  -- Use top bit of mdiv_opcodecode as flag for whether lower 2 bits are an
  -- MDIV_OPCODE or a LO_HI_OPCODE
  do_mdiv <= '1' when gen_opcode(3) = '1' and do_mdiv_op = '1' else '0';
  do_lo_hi <= '1' when gen_opcode(3) = '0' and do_mdiv_op = '1' else '0';
  
  -- used by control to stall pipeline
  -- Must fix stall logic if multiply becomes pipelined!
  mdiv_stall <=
    '1' when (lo_hi_opcode = MFLO_CODE or lo_hi_opcode = MFHI_CODE)
              and do_lo_hi = '1' and (div_ready and mult_ready) = '0' else
    '0';


  
  -- divide inputs
  div_numerator <= rs_reg_data;
  div_denominator <= rt_reg_data;
  div_is_unsigned <= '1' when mdiv_opcode = DIVU_CODE else '0';
  
  div_valid <=
    '1' when (mdiv_opcode = DIV_CODE or mdiv_opcode = DIVU_CODE) and
             do_mdiv = '1' else
    '0';

  
  -- multiply inputs
  mult_multiplicand <= rs_reg_data;
  mult_multiplier <= rt_reg_data;
  mult_is_unsigned <= '1' when mdiv_opcode = MULTU_CODE else '0';

  mult_valid <=
    '1' when (mdiv_opcode = MULT_CODE or mdiv_opcode = MULTU_CODE) and
             do_mdiv = '1' else
    '0';
  
  -- output
  mdiv_result <= lo_reg when lo_hi_opcode = MFLO_CODE else hi_reg;


  lo_hi_reg_proc:
  process (clk)
  begin
    if rising_edge(clk) then
      if lo_hi_opcode = MTLO_CODE and do_lo_hi = '1' then
        lo_reg <= rs_reg_data;
      elsif lo_hi_opcode = MTHI_CODE and do_lo_hi = '1' then
        hi_reg <= rs_reg_data;
      elsif div_done = '1' then
        lo_reg <= div_quotient;
        hi_reg <= div_remainder;
      elsif mult_done = '1' then
        lo_reg <= mult_result_lo;
        hi_reg <= mult_result_hi;
      end if;
    end if;
  end process;

  
  mult_gen: entity work.signed_multiplier
    generic map (N => 32)
    port map    (clk => clk,
                 mult_valid => mult_valid,
                 mult_ready => mult_ready,
                 mult_done => mult_done,
                 is_unsigned => mult_is_unsigned,
                 multiplicand => mult_multiplicand,
                 multiplier => mult_multiplier,
                 mult_result_hi => mult_result_hi,
                 mult_result_lo => mult_result_lo);

  
  div_gen: entity work.signed_divider
    generic map (N => 32)
    port map    (clk => clk,
                 valid => div_valid,
                 ready => div_ready,
                 done => div_done,
                 is_unsigned => div_is_unsigned,
                 numerator => div_numerator,
                 denominator => div_denominator,
                 quotient => div_quotient,
                 remainder => div_remainder);

end arch;
