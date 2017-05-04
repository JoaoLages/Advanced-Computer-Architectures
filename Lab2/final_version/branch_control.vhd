----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 15.02.2016 20:42:35
-- Design Name: 
-- Module Name: branch_control - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use WORK.MAIN_DEFINITIONS.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity branch_control is
  Port (
        PC       : in  STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        Cond     : in  STD_LOGIC_VECTOR( 2 downto 0);
        Offset   : in  STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        CondWord : in  STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);
        NextPC   : out STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0)
       );
end branch_control;

architecture Behavioral of branch_control is

signal PCIncrement : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
signal FlagZ, FlagN : STD_LOGIC;

begin

-- compute flags
FlagZ  <= '1' when CondWord=(WORD_WIDTH-1 downto 0=>'0') else '0';
FlagN  <= CondWord(WORD_WIDTH-1);

-- compute PC increment based on condition, flags and input offset value
PCIncrement <= Offset when Cond="111" else                               -- unconditional branch
               Offset when Cond="000" and FlagZ='1' else                 -- conditioned to WORD == 0
               Offset when Cond="001" and FlagZ='0' else                 -- conditioned to WORD <> 0
               Offset when Cond="010" and FlagN='1' else                 -- conditioned to WORD <  0
               Offset when Cond="011" and (FlagN='1' or FlagZ='1') else  -- conditioned to WORD <= 0
               Offset when Cond="100" and FlagN='0' and FlagZ='0'  else  -- conditioned to WORD >  0
               Offset when Cond="101" and FlagN='0' else                 -- conditioned to WORD >= 0
               std_logic_vector(to_unsigned(4, PC_WIDTH));               -- conditional branch (condition returns false)

-- compute nextPC
NextPC <= PC + PCIncrement;

end Behavioral;
