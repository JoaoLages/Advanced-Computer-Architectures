----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 25.02.2016 15:21:01
-- Design Name: 
-- Module Name: singleCoreProcessor_tb - Behavioral
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
use IEEE.STD_LOGIC_SIGNED.ALL;
use WORK.MAIN_DEFINITIONS.ALL;
USE STD.TEXTIO.ALL;
USE IEEE.STD_LOGIC_TEXTIO.ALL;
use WORK.TXT_UTIL.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity br_pred_tb is
--  Port ( );
end br_pred_tb;

architecture Behavioral of br_pred_tb is

component branch_control
  Port (
		reset: in std_logic;
		clk : in std_logic;
		ID_Br_PC : in STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
		EX_Br_PC : in STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
		ID_BrOffset : in STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
		Next_PC : out STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
		PB_WE : in STD_LOGIC;
		ID_Op_is_Branch: in STD_LOGIC;
		Taken : in STD_LOGIC
     );
end component;

-- global control signals
signal clk, reset : std_logic := '0';
-- instruction memory signals
signal ID_BR_PC : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(12,16));
signal EX_BR_PC : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(8,16));
signal ID_BrOffset : STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(4,16));
signal Next_PC : STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0) := (others=>'0');
signal PB_WE : STD_LOGIC :='0';
signal ID_Op_is_Branch: STD_LOGIC :='1';
signal Taken : STD_LOGIC := '0';


signal clk_cnt : integer;
constant clk_period : time := 100ns;

begin

brpred0: br_prediction port map (
      reset => reset,
		clk => clk,
		ID_Br_PC => ID_BR_PC,
		EX_Br_PC => EX_BR_PC,
		ID_BrOffset => ID_BrOffset,
		Next_PC => Next_PC, 
		PB_WE => PB_WE,
		ID_Op_is_Branch => ID_Op_is_Branch,
		Taken => Taken
     );


uReset: process
begin
    reset<='1';
    wait for 9*clk_period/4;
    reset <= '0';
    wait;
end process;

uClkGen: process
begin
    clk <= '1';
    wait for clk_period/2;
    clk <= '0';
    wait for clk_period/2;
end process;

process(clk)
variable my_line : line;
begin
    if rising_edge(clk) and reset='0' then
        clk_cnt <= clk_cnt + 1;
		 -- PB_WE<=ID_Op_is_Branch;
--        if MemWE = '1' then
           -- write(my_line, string'("Write to memory @ "));
          -- write(my_line, now); -- format time
          --  write(my_line, string'(" :: M("));
          --  write(my_line, hstr(MemAdd));
         --   write(my_line, string'(") <= "));
         --   write(my_line, hstr(MemWData));
          --  write(my_line, string'("    ("));
         --   write(my_line, conv_integer(MemWData));
        --    write(my_line, string'(")"));
         --   writeline(l_file, my_line);              -- write to display
        --end if;
    end if;
end process;

end Behavioral;
