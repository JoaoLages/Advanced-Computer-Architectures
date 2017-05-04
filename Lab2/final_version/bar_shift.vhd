----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date: 10.02.2016 12:04:57
-- Design Name:
-- Module Name: alu - Behavioral
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
use IEEE.NUMERIC_STD.ALL;
use WORK.MAIN_DEFINITIONS.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity bar_shift is
    Port (
       -- Operands
       OpA   : in STD_LOGIC_VECTOR  (WORD_WIDTH downto 0);
       OpB   : in STD_LOGIC_VECTOR  (4 downto 0);
       OpC   : in STD_LOGIC;
       -- Result
       Res  : out STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0)
     );
end bar_shift;

architecture Behavioral of bar_shift is

-- shifting operation signals
signal bshifterRes : std_logic_vector(WORD_WIDTH-1 downto 0);

begin

------------------------------------------------------------------------------
-- Barrel Shifter operations
------------------------------------------------------------------------------
uBarrelShifter: process(OpA, OpB(4 downto 0), OpC)
variable auxRes : std_logic_vector(2*WORD_WIDTH-1 downto 0);
variable NShifts : integer range 0 to WORD_WIDTH-1;
begin
    NShifts := TO_INTEGER(UNSIGNED(OpB(4 downto 0)));
    if OpC='0' then -- shift right
        auxRes := (WORD_WIDTH-1 downto 0=>OpA(WORD_WIDTH)) & OpA(WORD_WIDTH-1 downto 0);
        bshifterRes <= auxRes(WORD_WIDTH-1+NShifts downto NShifts);
        --bshifterRes <= (NShifts-1 downto 0=>OpA(WORD_WIDTH)) & OpA(WORD_WIDTH-1 downto NShifts);
        --auxRes := STD_LOGIC_VECTOR( SIGNED(To_bitvector(OpA)) sra NShifts );
    else -- shift left
        --auxRes := STD_LOGIC_VECTOR( SIGNED(To_bitvector(OpA)) sla NShifts );
        auxRes := OpA(WORD_WIDTH-1 downto 0) & (WORD_WIDTH-1 downto 0=>'0');
        bshifterRes <= auxRes(2*WORD_WIDTH-1-NShifts downto WORD_WIDTH-NShifts);
        --bshifterRes <= auxRes(WORD_WIDTH-1-NShifts downto 0) & (NShifts-1 downto 0=>OpA(WORD_WIDTH));
    end if;
    --bshifterRes <= auxRes(WORD_WIDTH-1 downto 0);
end process;

------------------------------------------------------------------------------
-- Compute output
------------------------------------------------------------------------------
Res <= bshifterRes;

end Behavioral;
