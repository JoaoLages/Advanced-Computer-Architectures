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

entity mul is
    Port (
       -- Operands
       OpA   : in STD_LOGIC_VECTOR  (WORD_WIDTH downto 0);
       OpB   : in STD_LOGIC_VECTOR  (WORD_WIDTH downto 0);
       OpC   : in STD_LOGIC;
       -- Result
       Res  : out STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0)
     );
end mul;

architecture Behavioral of mul is

-- multiplying operation signals
signal MulRes : std_logic_vector(WORD_WIDTH-1 downto 0);

begin

------------------------------------------------------------------------------
-- Multiplication operations
------------------------------------------------------------------------------
uMultiplier: process(OpA, OpB, OpC)
variable auxRes : std_logic_vector(2*WORD_WIDTH+1 downto 0);
begin
    auxRes := OpA * OpB;
    if OpC='0' then
        MulRes <= auxRes(WORD_WIDTH-1 downto 0);
    else
        MulRes <= auxRes(2*WORD_WIDTH-1 downto WORD_WIDTH);
    end if;
end process;

------------------------------------------------------------------------------
-- Compute output
------------------------------------------------------------------------------
Res <= MulRes;

end Behavioral;
