----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 24.02.2016 17:27:20
-- Design Name: 
-- Module Name: SingleCoreProcessor - Behavioral
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
use WORK.MAIN_DEFINITIONS.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity SingleCoreProcessor is
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
end SingleCoreProcessor;

architecture Behavioral of SingleCoreProcessor is

signal PCval, MemAddress : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
signal I : STD_LOGIC_VECTOR(31 downto 0);
signal MemWriteEnable : STD_LOGIC;
signal MemWriteData, MemReadData : STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);
begin

PC <= PCval;
MemAdd <= MemAddress;
MemWData <= MemWriteData;
MemWE <= MemWriteEnable;

Core0: simpleCore port map (
        clk            => clk,
        reset          => reset,
        PC             => PCval,
        I              => I,
        MemAddress     => MemAddress,
        MemWriteEnable => MemWriteEnable,
        MemWriteData   => MemWriteData,
        MemReadData    => MemReadData
       );

CoreMem0: DualPortMemory port map (
	    clk     => clk,
	    Addr_A  => PCval,
	    DO_A    => I,
	    WE_B    => MemWriteEnable,
	    Addr_B  => MemAddress,
	    DI_B    => MemWriteData,
	    DO_B    => MemReadData
       );

end Behavioral;
