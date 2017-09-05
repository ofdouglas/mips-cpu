library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

--  DATA BUS INTERFACE (big endian)
------------------------------------------------------
--  |31       24|23       16|15        8|7          0|
--  |   byte0   |   byte1   |   byte2   |   byte 3   |
------------------------------------------------------
--   lane_en(0)                           lane_en(3) 
--   address N                            address N+3
--   rt_reg MSB                           rt_reg LSB
--
-- To make it easier to reason about endianness, all read/write ports
-- (including register RT - the source for STORE and destination for LOAD)
-- are managed internally as an array of bytes (0 to 3).
-- 
-- The MSB of the RT register (31 downto 24) is always referenced here as
-- rt_load_bytes(0), regardless of the BigEndianCPU mode setting.
-- 
-- The instruction lw $rt 0($rs) maps the byte at address [$rs + 0] into
-- dmem_load_data(0) when in Big Endian mode, and into dmem_load_data(3) when
-- in Little Endian mode.
-- 

entity dmem_interface is
  port (
    -- CPU Control Port
    gen_opcode        : in std_logic_vector(3 downto 0);
    dmem_addr_error   : out std_logic;
    big_endian_mode   : in std_logic;
    
    -- CPU Data Port
    dmem_address      : in unsigned(31 downto 0);
    dmem_load_data    : out std_logic_vector(31 downto 0);
    dmem_store_data   : in std_logic_vector(31 downto 0);

    -- Data Bus Control Port
    dbus_lane_en      : out std_logic_vector(0 to 3);
    
    -- Data Bus Data Port
    dbus_addr         : out unsigned(31 downto 0);
    dbus_rd_data      : in std_logic_vector(31 downto 0);
    dbus_wr_data      : out std_logic_vector(31 downto 0)
    );
end dmem_interface;

architecture arch of dmem_interface is

  alias dmem_opcode   : std_logic_vector(2 downto 0) is gen_opcode(2 downto 0);
  alias dmem_unsigned : std_logic is gen_opcode(3);
  
  type mem_word_type is array(0 to 3) of std_logic_vector(7 downto 0);
  
  signal dbus_rd_bytes    : mem_word_type;
  signal dbus_wr_bytes    : mem_word_type;
  signal dmem_load_bytes  : mem_word_type;
  signal dmem_store_bytes : mem_word_type;

begin

  dbus_addr <= dmem_address;
  
  -- Convert Big Endian words (input) to array of bytes (internal use)
  dbus_rd_bytes(0) <= dbus_rd_data(31 downto 24);
  dbus_rd_bytes(1) <= dbus_rd_data(23 downto 16);
  dbus_rd_bytes(2) <= dbus_rd_data(15 downto 8);
  dbus_rd_bytes(3) <= dbus_rd_data(7 downto 0);
  
  dmem_store_bytes(0) <= dmem_store_data(31 downto 24);
  dmem_store_bytes(1) <= dmem_store_data(23 downto 16);
  dmem_store_bytes(2) <= dmem_store_data(15 downto 8);
  dmem_store_bytes(3) <= dmem_store_data(7 downto 0);

  -- Convert array of bytes (internal use) to Big Endian words (output)
  dbus_wr_data <= dbus_wr_bytes(0) & dbus_wr_bytes(1) &
                  dbus_wr_bytes(2) & dbus_wr_bytes(3);
  
  dmem_load_data <= dmem_load_bytes(0) & dmem_load_bytes(1) &
                  dmem_load_bytes(2) & dmem_load_bytes(3);

  
  -- Enforce word and halfword alignment (ignored by containing module
  -- when (do_load or do_store = '0').
  dmem_addr_error <= '1' when
    (dmem_opcode = LS_HALF_CODE and dmem_address(0) = '1') or
    (dmem_opcode = LS_WORD_CODE and dmem_address(1 downto 0) /= "00")
    else '0';

  

  -- Set byte lane enables and connect read and write port bytes as
  -- required for the given load/store operation. The load and store 
  -- port byte muxes are always set, but one or both will be ignored
  -- by the bus system (not in this entity) based on the 'do_load' and
  -- 'do_store' signals.
  load_store_proc:
  process (dmem_opcode, dmem_unsigned, big_endian_mode,
           dmem_address, dbus_rd_bytes, dmem_store_bytes)
    variable offset : integer range 0 to 3;
    variable ms_bit : std_logic;            -- most significant bit
  begin

    -- Defaults to prevent latches from being inferred
    dbus_lane_en <= (others => '0');
    dmem_load_bytes <= (others => (others => '0'));
    dbus_wr_bytes <= dmem_store_bytes;

    -- Convenience variable
    offset := to_integer(unsigned(dmem_address(1 downto 0)));

    
    case dmem_opcode is

      -- Load/Store Byte
      when LS_BYTE_CODE =>
        dbus_lane_en(offset) <= '1';
        dmem_load_bytes(3) <= dbus_rd_bytes(offset);
        ms_bit := dbus_rd_bytes(offset)(7);
        dmem_load_bytes(0 to 2) <=
          (others => (others => ms_bit and not dmem_unsigned));
        dbus_wr_bytes(offset) <= dmem_store_bytes(3);

      -- Load/Store Halfword  
      when LS_HALF_CODE =>
        if dmem_address(1) = '0' then
          dbus_lane_en <= "1100";
          
          if big_endian_mode = '1' then
            dmem_load_bytes(2 to 3) <= dbus_rd_bytes(0 to 1);
            ms_bit := dbus_rd_bytes(0)(7);
            dbus_wr_bytes(0 to 1) <= dmem_store_bytes(2 to 3);      
          else
            dmem_load_bytes(2) <= dbus_rd_bytes(1);
            dmem_load_bytes(3) <= dbus_rd_bytes(0);
            dbus_wr_bytes(0) <= dmem_store_bytes(3);
            dbus_wr_bytes(1) <= dmem_store_bytes(2);
            ms_bit := dbus_rd_bytes(1)(7);
          end if;
        else
          dbus_lane_en <= "0011";
          
          if big_endian_mode = '1' then
            dmem_load_bytes(2 to 3) <= dbus_rd_bytes(2 to 3);
            ms_bit := dbus_rd_bytes(1)(7);
            dbus_wr_bytes(2 to 3) <= dmem_store_bytes(2 to 3);
          else
            dmem_load_bytes(2) <= dbus_rd_bytes(3);
            dmem_load_bytes(3) <= dbus_rd_bytes(2);
            dbus_wr_bytes(2) <= dmem_store_bytes(3);
            dbus_wr_bytes(3) <= dmem_store_bytes(2);
            ms_bit := dbus_rd_bytes(3)(7);
          end if;
        end if;

        dmem_load_bytes(0 to 1) <=
          (others => (others => ms_bit and not dmem_unsigned));
        
      -- Load/Store Word
      when LS_WORD_CODE =>
        dbus_lane_en <= "1111";
        if big_endian_mode = '1' then
          dmem_load_bytes <= dbus_rd_bytes;
          dbus_wr_bytes <= dmem_store_bytes;
        else
          for i in 0 to 3 loop
            dmem_load_bytes(i) <= dbus_rd_bytes(3 - i);
            dbus_wr_bytes(i) <= dmem_store_bytes(3 - i);
          end loop;
        end if;

      -- Load/Store Word Left
      -- Left: Work from given byte address towards least significant end of
      -- memory word, using bytes from left end to right end of register.
      when LS_LEFT_CODE =>
        if big_endian_mode = '1' then
          case offset is
            -- Should be this, but Vivado doesn't do 'complex assignment'...
            -- dbus_lane_en(offset to 3) <= (others => '1');
            -- dbus_wr_bytes(offset to 3) <= dmem_store_bytes(0 to 3 - offset);
            -- dmem_load_bytes(0 to 3 - offset) <= dbus_rd_bytes(offset to 3);
            when 0 =>
              dbus_wr_bytes(0 to 3) <= dmem_store_bytes(0 to 3);
              dmem_load_bytes(0 to 3) <= dbus_rd_bytes(0 to 3);
              dbus_lane_en <= "1111";
            when 1 =>
              dbus_wr_bytes(1 to 3) <= dmem_store_bytes(0 to 2);
              dmem_load_bytes(0 to 2) <= dbus_rd_bytes(1 to 3);
              dbus_lane_en <= "0111";
            when 2 =>
              dbus_wr_bytes(2 to 3) <= dmem_store_bytes(0 to 1);
              dmem_load_bytes(0 to 1) <= dbus_rd_bytes(2 to 3);
              dbus_lane_en <= "0011";
            when 3 =>
              dbus_wr_bytes(3 to 3) <= dmem_store_bytes(0 to 0);
              dmem_load_bytes(0 to 0) <= dbus_rd_bytes(3 to 3);
              dbus_lane_en <= "0001";
          end case;
        else
          case offset is
            -- Should be this, but Vivado doesn't do 'complex assignment'...
            -- dbus_lane_en(0 to offset) <= (others => '1');
            -- for i in 0 to offset loop
            --  dbus_wr_bytes(i) <= dmem_store_bytes(offset - i);
            --  dmem_load_bytes(offset - i) <= dbus_rd_bytes(i);
            -- end loop;
            when 0 =>
              dbus_lane_en <= "1000";
              dbus_wr_bytes(0) <= dmem_store_bytes(0);
              dmem_load_bytes(0) <= dbus_rd_bytes(0);
            when 1 =>
              dbus_lane_en <= "1100";
              dbus_wr_bytes(0) <= dmem_store_bytes(1);
              dbus_wr_bytes(1) <= dmem_store_bytes(0);
              dmem_load_bytes(0) <= dbus_rd_bytes(1);
              dmem_load_bytes(1) <= dbus_rd_bytes(0);
            when 2 =>
              dbus_lane_en <= "1110";
              dbus_wr_bytes(0) <= dmem_store_bytes(2);
              dbus_wr_bytes(1) <= dmem_store_bytes(1);
              dbus_wr_bytes(2) <= dmem_store_bytes(0);
              dmem_load_bytes(0) <= dbus_rd_bytes(2);
              dmem_load_bytes(1) <= dbus_rd_bytes(1);
              dmem_load_bytes(2) <= dbus_rd_bytes(0);
            when 3 =>
              dbus_lane_en <= "1111";
              dbus_wr_bytes(0) <= dmem_store_bytes(3);
              dbus_wr_bytes(1) <= dmem_store_bytes(2);
              dbus_wr_bytes(2) <= dmem_store_bytes(1);
              dbus_wr_bytes(3) <= dmem_store_bytes(0);
              dmem_load_bytes(0) <= dbus_rd_bytes(3);
              dmem_load_bytes(1) <= dbus_rd_bytes(2);
              dmem_load_bytes(2) <= dbus_rd_bytes(1);
              dmem_load_bytes(3) <= dbus_rd_bytes(0);
          end case;
        end if;
        
      -- Load/Store Word Right
      -- Right: Work from given byte address towards most significant end of
      -- memory word, using bytes from right end to left end of register.
      when LS_RIGHT_CODE =>
        if big_endian_mode = '1' then
          case offset is
            -- Should be this, but Vivado doesn't do 'complex assignment'...
            -- dbus_lane_en(0 to offset) <= (others => '1');
            -- dbus_wr_bytes(0 to offset) <= dmem_store_bytes(3 - offset to 3);
            -- dmem_load_bytes(3 - offset to 3) <= dbus_rd_bytes(0 to offset);
            when 0 =>
              dbus_lane_en <= "1000";
              dbus_wr_bytes(0 to 0) <= dmem_store_bytes(3 to 3);
              dmem_load_bytes(3 to 3) <= dbus_rd_bytes(0 to 0);
            when 1 =>
              dbus_lane_en <= "1100";
              dbus_wr_bytes(0 to 1) <= dmem_store_bytes(2 to 3);
              dmem_load_bytes(2 to 3) <= dbus_rd_bytes(0 to 1);
            when 2 =>
              dbus_lane_en <= "1110";
              dbus_wr_bytes(0 to 2) <= dmem_store_bytes(1 to 3);
              dmem_load_bytes(1 to 3) <= dbus_rd_bytes(0 to 2);
            when 3 =>
              dbus_lane_en <= "1111";
              dbus_wr_bytes(0 to 3) <= dmem_store_bytes(0 to 3);
              dmem_load_bytes(0 to 3) <= dbus_rd_bytes(0 to 3);
          end case;
        else
          case offset is
          -- Should be this, but Vivado doesn't do 'complex assignment'...
          -- dbus_lane_en(offset to 3) <= (others => '1');
          -- for i in offset to 3 loop
          --   dbus_wr_bytes(i) <= dmem_store_bytes(3 + offset - i);
          --   dmem_load_bytes(3 + offset - i) <= dbus_rd_bytes(i);
          -- end loop;
            when 0 =>
              dbus_lane_en <= "1111";
              dbus_wr_bytes(0) <= dmem_store_bytes(3);
              dbus_wr_bytes(1) <= dmem_store_bytes(2);
              dbus_wr_bytes(2) <= dmem_store_bytes(1);
              dbus_wr_bytes(3) <= dmem_store_bytes(0);
              dmem_load_bytes(0) <= dbus_rd_bytes(3);
              dmem_load_bytes(1) <= dbus_rd_bytes(2);
              dmem_load_bytes(2) <= dbus_rd_bytes(1);
              dmem_load_bytes(3) <= dbus_rd_bytes(0);
            when 1 =>
              dbus_lane_en <= "0111";
              dbus_wr_bytes(1) <= dmem_store_bytes(3);
              dbus_wr_bytes(2) <= dmem_store_bytes(2);
              dbus_wr_bytes(3) <= dmem_store_bytes(1);
              dmem_load_bytes(1) <= dbus_rd_bytes(3);
              dmem_load_bytes(2) <= dbus_rd_bytes(2);
              dmem_load_bytes(3) <= dbus_rd_bytes(1);
            when 2 =>
              dbus_lane_en <= "0011";
              dbus_wr_bytes(2) <= dmem_store_bytes(3);
              dbus_wr_bytes(3) <= dmem_store_bytes(2);
              dmem_load_bytes(2) <= dbus_rd_bytes(3);
              dmem_load_bytes(3) <= dbus_rd_bytes(2);
            when 3 =>
              dbus_lane_en <= "0001";
              dbus_wr_bytes(3) <= dmem_store_bytes(3);
              dmem_load_bytes(3) <= dbus_rd_bytes(3);
          end case;
        end if;

      when others =>
        -- do nothing
        
    end case;
  end process;


end arch;

