library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Simple enough to not need it's own file right now, but will probably get 
-- complicated in future versions.


entity imem_interface is
  port (
    -- CPU Data port
    instruction     : out std_logic_vector(31 downto 0);
    
    -- CPU Control port
    big_endian_mode : in std_logic;
    
    -- IBUS Data Port
    ibus_rd_data    : in std_logic_vector(31 downto 0)
    );
end imem_interface;

architecture arch of imem_interface is

begin

  -- Swap byte order if in little endian mode
  process (big_endian_mode, ibus_rd_data, big_endian_mode)
  begin
    
    if big_endian_mode = '1' then
      instruction <= ibus_rd_data;
    else
      instruction <= ibus_rd_data(7 downto 0) & ibus_rd_data(15 downto 8) &
                     ibus_rd_data(23 downto 16) & ibus_rd_data(31 downto 24);
    end if;
  end process;

end arch;
