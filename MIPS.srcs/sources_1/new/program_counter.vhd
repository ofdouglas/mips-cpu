library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.MIPS_r3000_ISA.ALL;


entity program_counter is
  port (clk              : in std_logic;
        reset            : in std_logic;
        exception_flush  : in std_logic;
        exc_vector_mux   : exc_vector_mux_type;
        boot_exc_vectors : in std_logic;
        pc_next          : in unsigned(31 downto 0);
        pc_current       : out unsigned(31 downto 0);
        pc_plus_4        : out unsigned(31 downto 0);
        pc_enable        : in std_logic
        );
end program_counter;

architecture arch of program_counter is

  signal pc : unsigned(31 downto 0) := EXC_RESET_VADDR;

  signal exception_vector : unsigned(31 downto 0);
  
begin


  pc_plus_4 <= pc + (X"0000_0004");
  pc_current <= pc;

  exc_vector_mux_proc:
  process (exc_vector_mux, boot_exc_vectors)
  begin
    case exc_vector_mux is
      
      when EXC_GENERAL =>
        if boot_exc_vectors = '1' then
          exception_vector <= EXC_GENERAL_NOCACHE_VADDR;
        else
          exception_vector <= EXC_GENERAL_VADDR;
        end if;

      when EXC_TLB_MISS =>
        if boot_exc_vectors = '1' then
          exception_vector <= EXC_TLB_MISS_NOCACHE_VADDR;
        else
          exception_vector <= EXC_TLB_MISS_VADDR;
        end if;

      when EXC_RESET =>
        exception_vector <= EXC_RESET_VADDR;
        
    end case;
  end process;
  
  
  pc_reg_proc:
  process (clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        pc <= EXC_RESET_VADDR;
      elsif exception_flush = '1' then
        pc <= exception_vector;
      elsif pc_enable = '1' then
        pc <= pc_next;
      end if;
    end if;

    -- Improve performance by getting the synthesis tool to trim these bits.
    -- Making the PC (31 downto 2) would be inconvenient for simulation, etc.
    -- The PC is always supposed to be 4-byte aligned, so it's safe to force
    -- alignment here and catch alignment errors in 'pc_next' (ID stage).
    
    pc(1 downto 0) <= "00";
    
  end process;
    
end arch;
