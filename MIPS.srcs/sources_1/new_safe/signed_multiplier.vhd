library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity signed_multiplier is
  generic (N : integer := 32);
  port (clk            : in std_logic;
        -- Control port
        mult_valid     : in std_logic;
        mult_ready     : out std_logic;
        mult_done      : out std_logic;
        -- Data port
        is_unsigned    : in std_logic;
        multiplicand   : in std_logic_vector(N-1 downto 0);
        multiplier     : in std_logic_vector(N-1 downto 0);
        mult_result_hi : out std_logic_vector(N-1 downto 0);
        mult_result_lo : out std_logic_vector(N-1 downto 0)
        );
end signed_multiplier;



architecture arch of signed_multiplier is
  
  COMPONENT mult_gen_0
    PORT (
      CLK : IN STD_LOGIC;
      A : IN STD_LOGIC_VECTOR(32 DOWNTO 0);
      B : IN STD_LOGIC_VECTOR(32 DOWNTO 0);
      CE : IN STD_LOGIC;
      P : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
      );
  END COMPONENT;

  -- Use an extra bit to handle both signed and unsigned
  signal A  : std_logic_vector(N downto 0);
  signal B  : std_logic_vector(N downto 0);
  signal P  : std_logic_vector(N*2 -1 downto 0);
  signal CE : std_logic;

  -- When configured with N pipeline stages, the Xilinx mult_gen 'P' output
  -- is valid after N-1 clocks.
  constant MULT_PIPE_STAGES : integer := 6;
  constant COUNTER_MAX      : integer := MULT_PIPE_STAGES - 2;
  type mult_state_type is (IDLE_STATE, BUSY_STATE, DONE_STATE);
  
  signal mult_state : mult_state_type := IDLE_STATE;
  signal counter   : integer range COUNTER_MAX downto 0;
  
begin

  -- Controls
  mult_ready <= '1' when mult_state = IDLE_STATE else '0';
  mult_done <= '1' when mult_state = DONE_STATE else '0';
  ce <= '1' when mult_state = IDLE_STATE and mult_valid = '1' else
        '1' when mult_state = BUSY_STATE else
        '0';

  -- Data inputs
  A <= '0' & multiplicand when is_unsigned = '1' else
       multiplicand(N-1) & multiplicand;
  
  B <= '0' & multiplier when is_unsigned = '1' else
       multiplier(N-1) & multiplier;

  -- Data output
  mult_result_hi <= P(63 downto 32);
  mult_result_lo <= P(31 downto 0);

  
  mult_ctrl_proc:
  process (clk)
  begin
    if rising_edge(clk) then
      
      case mult_state is
        when IDLE_STATE =>
          if mult_valid = '1' then
            counter <= counter_max;
            mult_state <= busy_state;
          end if;

        when BUSY_STATE =>
          counter <= counter - 1;
          if counter = 0 then
            mult_state <= done_state;
          end if;

        when DONE_STATE =>
          mult_state <= IDLE_STATE;
          -- just for output 'done' pulse
          
      end case;
    end if;
  end process;
  
  pipelined_mult : mult_gen_0
    PORT MAP (
      CLK => CLK,
      A => A,
      B => B,
      CE => CE,
      P => P
      );

end arch;


