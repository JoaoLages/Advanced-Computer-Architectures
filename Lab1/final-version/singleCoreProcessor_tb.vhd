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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity singleCoreProcessor_tb is
--  Port ( );
end singleCoreProcessor_tb;

architecture Behavioral of singleCoreProcessor_tb is

component SingleCoreProcessor
  Port (
      -- global control signals
      clk      : in  STD_LOGIC;
      reset    : in  STD_LOGIC;
      -- instruction memory signals
      PC       : out STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
      -- data memory signals
      MemAdd   : out STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
      MemWE    : out STD_LOGIC; 
      MemWData : out STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0) 
     );
end component;

-- global control signals
signal clk, reset : std_logic := '0';
-- instruction memory signals
signal PC : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0) := (others=>'0');
signal I : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0');
-- data memory signals
signal MemAdd   : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0) := (others=>'0');
signal MemWE    : STD_LOGIC := '0'; 
signal MemWData : STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0) := (others=>'0');

signal clk_cnt : integer;
file l_file: TEXT open write_mode is "mem1.log";
constant clk_period : time := 10ns;

begin

proc0: SingleCoreProcessor port map (
      clk      => clk,
      reset    => reset,
      -- instruction memory signals
      PC       => PC,
      -- data memory signals
      MemAdd   => MemAdd,
      MemWE    => MemWE, 
      MemWData => MemWData 
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
    if rising_edge(clk) then
        clk_cnt <= clk_cnt + 1;
        if MemWE = '1' then
            write(my_line, string'("Write to memory @ "));
            write(my_line, now); -- format time
            write(my_line, string'(" :: M("));
            write(my_line, hstr(MemAdd));
            write(my_line, string'(") <= "));
            write(my_line, hstr(MemWData));
            write(my_line, string'("    ("));
            write(my_line, conv_integer(MemWData));
            write(my_line, string'(")"));
            writeline(l_file, my_line);              -- write to display
        end if;
    end if;
end process;

end Behavioral;
