library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Note that (INT_MIN / -1) = INT_MIN




entity signed_divider is
  generic (N : integer := 4);   -- word size (number of bits)
  port    (clk         : in std_logic;
           valid       : in std_logic;
           ready       : out std_logic;
           done        : out std_logic;
           is_unsigned : in std_logic;
           numerator   : in std_logic_vector(N-1 downto 0);
           denominator : in std_logic_vector(N-1 downto 0);
           quotient    : out std_logic_vector(N-1 downto 0);
           remainder   : out std_logic_vector(N-1 downto 0));
end signed_divider;


architecture restoring_div of signed_divider is

-- 1. If doing signed division, save signs of inputs and invert any
--    negative inputs.
-- 2. Perform unsigned restoring division.
-- 3. If doing signed division, make the remainder have the sign of the
--    numerator. Invert the quotient if the inputs had different signs.
--
-- Arithmetic registers are N+1 bits wide to accomodate signed->unsigned
-- converstion (We must be able to make INT_MIN positive).  
--
-- The subtractor is N+2 bits wide to do unsigned subtraction without the
-- posibility of underflow.


  -- Arithmetic Registers:
  
  -- NOTE: we need the default assignment: 'U' inputs to combinational logic
  -- yield 'X' results...
  signal denm : unsigned(N downto 0) := (others => '0');
  signal quot : unsigned(N downto 0) := (others => '0');
  signal remn : unsigned(N downto 0) := (others => '0');
  signal denm_next : unsigned(N downto 0);
  signal quot_next : unsigned(N downto 0);
  signal remn_next : unsigned(N downto 0);
  
  -- Intermediate Results:
  signal remn_shift  : unsigned(N downto 0);
  signal sub_result  : unsigned(N+1 downto 0);
  signal sub_input_A : unsigned(N+1 downto 0);
  signal sub_input_B : unsigned(N+1 downto 0);

  -- Count through N+1 shift-subtract iterations
  constant COUNTER_MAX : integer := N;

  type div_state_type is (IDLE_STATE, PRE_ADJUST_STATE, DIVIDE_STATE,
                          ADJUST_QUO_STATE, ADJUST_REM_STATE, DONE_STATE);

  -- Control Registers:
  signal div_state   : div_state_type := IDLE_STATE;
  signal counter     : integer range COUNTER_MAX downto 0;
  signal numr_sign   : std_logic;
  signal denm_sign   : std_logic;
  signal do_unsigned : std_logic;
  
begin

  -- Control Signals
  ready <= '1' when div_state = IDLE_STATE else '0';
  done <= '1' when div_state = DONE_STATE else '0';

  -- Arithmetic Result: invalid until 'done' is asserted!
  quotient  <= std_logic_vector(quot(quot'high -1 downto 0));
  remainder <= std_logic_vector(remn(remn'high -1 downto 0));

  -- N+2 bit Subtractor
  sub_result <= sub_input_A - sub_input_B;

  remn_shift <= remn(remn'high - 1 downto 0) & quot(quot'high);
  
  sequential_proc:
  process (clk)
  begin
    if rising_edge(clk) then

      -- Arithmetic register assignments
      denm <= denm_next;
      quot <= quot_next;
      remn <= remn_next;

      -- FSM control
      case div_state is

        when IDLE_STATE =>
          if valid = '1' then
            counter <= COUNTER_MAX;
            denm_sign <= denominator(denominator'high);
            numr_sign <= numerator(numerator'high);
            do_unsigned <= is_unsigned;

            if is_unsigned = '1' then
              div_state <= DIVIDE_STATE;
            else
              div_state <= PRE_ADJUST_STATE;
            end if;
          end if;

        when PRE_ADJUST_STATE =>
          div_state <= DIVIDE_STATE;

        when DIVIDE_STATE =>
          counter <= counter - 1;
          if counter = 0 then
            if do_unsigned = '1' then
              div_state <= DONE_STATE;
            else
              div_state <= ADJUST_QUO_STATE;
            end if;
          end if;

        when ADJUST_QUO_STATE =>
          div_state <= ADJUST_REM_STATE;

        when ADJUST_REM_STATE =>
          div_state <= DONE_STATE;

        when DONE_STATE =>
          div_state <= IDLE_STATE;
      end case;
      
    end if;  
  end process;

  
  -- Next-state assignments
  combinatorial_proc:
  process (valid, is_unsigned, numerator, denominator,
           div_state, sub_result, remn_shift,
           quot, remn, denm, numr_sign, denm_sign)
  begin

    -- defaults to avoid latches
    denm_next <= denm;
    quot_next <= quot;
    remn_next <= remn;
    
    -- maybe not most efficient defaults ?
    sub_input_A <= (others => '0'); 
    sub_input_B <= (others => '0');
    
      case div_state is

        when IDLE_STATE =>
          -- Load control and (sign or zero extended) arithmetic registers.
          -- If doing signed division and numerator < 0, invert it.
          
          if valid = '1' then
            remn_next <= (others => '0');

            if is_unsigned = '1' then
              denm_next <= unsigned('0' & denominator);
              quot_next <= unsigned('0' & numerator);
            else
              quot_next <= unsigned(numerator(numerator'high) & numerator);

              -- Invert the denominator if it's negative
              if denominator(denominator'high) = '1' then
                sub_input_A <= (others => '0');
                sub_input_B <= unsigned("11" & denominator);
                denm_next <= unsigned(sub_result(sub_result'high -1 downto 0));
              else
                denm_next <= unsigned('0' & denominator);
              end if;
            end if;
          end if;

          
        when PRE_ADJUST_STATE =>
          -- Invert the quotient if it's negative.
          -- Unsigned division skips this state.

          if quot(quot'high) = '1' then
            sub_input_A <= (others => '0');
            sub_input_B <= "1" & quot;
            quot_next <= sub_result(sub_result'high -1 downto 0);
          end if;


        when DIVIDE_STATE =>
          -- Generate quotient by shifting and subtracting N+1 times.
          -- At this point, all quantities are positive

          quot_next(quot'high downto 1) <= quot(quot'high -1 downto 0);
          
          sub_input_A <= '0' & remn_shift;
          sub_input_B <= '0' & denm;
          
          -- Negative result implies that demn < remn_shift
          if (sub_result(sub_result'high) = '1') then
            remn_next <= remn_shift;
            quot_next(0) <= '0';
          else
            remn_next <= sub_result(sub_result'high -1 downto 0);
            quot_next(0) <= '1';
          end if;

          
        when ADJUST_QUO_STATE =>
          -- Negate quotient, if necessary (unsigned division skips this state)

          if numr_sign /= denm_sign then
            sub_input_A <= (others => '0');
            sub_input_B <= '0' & quot;
            quot_next <= sub_result(sub_result'high -1 downto 0);
          end if;


        when ADJUST_REM_STATE =>
          -- Negate remainder, if necessary (unsigned division skips this state)

          if numr_sign = '1' then
            sub_input_A <= (others => '0');
            sub_input_B <= '0' & remn;
            remn_next <= sub_result(sub_result'high -1 downto 0);
          end if;

        when DONE_STATE =>
          -- do nothing
      end case;
          
  end process;

end restoring_div;

