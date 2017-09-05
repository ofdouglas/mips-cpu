library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MIPS_r3000_ISA.ALL;

-- FORWARDING targets:
--  ID_rs_data, ID_rt_data (for JUMP, BRANCH, reg read from WB results)
--  EX_rs_data, EX_rt_data (for ALU, MDIV, Shift)
--  MEM_rt_data (for STORE)

entity fwd_unit is
  port (EX_MEM_rgf_wr_en    : in std_logic;
        MEM_WB_rgf_wr_en    : in std_logic;
        EX_MEM_reg_dest_num : in unsigned(4 downto 0);
        MEM_WB_reg_dest_num : in unsigned(4 downto 0);
        ID_rs_reg_num       : in unsigned(4 downto 0);
        ID_rt_reg_num       : in unsigned(4 downto 0);
        EX_rs_reg_num       : in unsigned(4 downto 0);
        EX_rt_reg_num       : in unsigned(4 downto 0);
        MEM_rt_reg_num      : in unsigned(4 downto 0);

        ID_rs_fwd_mux       : out rs_fwd_mux_type;
        ID_rt_fwd_mux       : out rt_fwd_mux_type;
        EX_rs_fwd_mux       : out rs_fwd_mux_type;
        EX_rt_fwd_mux       : out rt_fwd_mux_type;
        MEM_rt_fwd_mux      : out mem_rt_fwd_mux_type
        );
  
end fwd_unit;

architecture arch of fwd_unit is

begin

  -- If-THEN priority semantics matter: FWD_RS_FROM_EX trumps FWD_RS_FROM_MEM!
  forwarding_mux_proc:
  process (EX_MEM_rgf_wr_en, MEM_WB_rgf_wr_en, EX_MEM_reg_dest_num,
           MEM_WB_reg_dest_num, ID_rs_reg_num, ID_rt_reg_num,
           EX_rs_reg_num, EX_rt_reg_num, MEM_rt_reg_num)
  begin
    ID_rs_fwd_mux <= RS_LOCAL;
    ID_rt_fwd_mux <= RT_LOCAL;
    EX_rs_fwd_mux <= RS_LOCAL;
    EX_rt_fwd_mux <= RT_LOCAL;
    MEM_rt_fwd_mux <= RT_LOCAL;
    
    if EX_MEM_rgf_wr_en = '1' and EX_MEM_reg_dest_num = ID_rs_reg_num then
      ID_rs_fwd_mux <= FWD_RS_FROM_EX;
    elsif MEM_WB_rgf_wr_en = '1' and MEM_WB_reg_dest_num = ID_rs_reg_num then
      ID_rs_fwd_mux <= FWD_RS_FROM_MEM;
    end if;
    
    if EX_MEM_rgf_wr_en = '1' and EX_MEM_reg_dest_num = ID_rt_reg_num then
      ID_rt_fwd_mux <= FWD_RT_FROM_EX;
    elsif MEM_WB_rgf_wr_en = '1' and MEM_WB_reg_dest_num = ID_rt_reg_num then
      ID_rt_fwd_mux <= FWD_RT_FROM_MEM;
    end if;
    
    if EX_MEM_rgf_wr_en = '1' and EX_MEM_reg_dest_num = EX_rs_reg_num then
      EX_rs_fwd_mux <= FWD_RS_FROM_EX;
    elsif MEM_WB_rgf_wr_en = '1' and MEM_WB_reg_dest_num = EX_rs_reg_num then
      EX_rs_fwd_mux <= FWD_RS_FROM_MEM;
    end if;
    
    if EX_MEM_rgf_wr_en = '1' and EX_MEM_reg_dest_num = EX_rt_reg_num then
      EX_rt_fwd_mux <= FWD_RT_FROM_EX;
    elsif MEM_WB_rgf_wr_en = '1' and MEM_WB_reg_dest_num = EX_rt_reg_num then
      EX_rt_fwd_mux <= FWD_RT_FROM_MEM;
    end if;

    if MEM_WB_rgf_wr_en = '1' and MEM_WB_reg_dest_num = MEM_rt_reg_num then
      MEM_rt_fwd_mux <= FWD_RT_FROM_MEM;
    end if;

  end process;


end arch;
