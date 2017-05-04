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

entity alu is
    Port ( 
       -- Control
       CTRL  : in STD_LOGIC_VECTOR  ( 2 downto 0);
       -- Operands 
       OpA   : in STD_LOGIC_VECTOR  (WORD_WIDTH downto 0);
       OpB   : in STD_LOGIC_VECTOR  (WORD_WIDTH downto 0);
       OpC   : in STD_LOGIC;
       -- Result
       Res  : out STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0);
       -- flags
       FlagC : out STD_LOGIC
     );
end alu;

architecture Behavioral of alu is

-- adding/comparison operation signals
signal AddRes : std_logic_vector(WORD_WIDTH-1 downto 0);
signal AddC   : std_logic;

-- multiplying operation signals
signal MulRes : std_logic_vector(WORD_WIDTH-1 downto 0);

-- shifting operation signals
signal bshifterRes : std_logic_vector(WORD_WIDTH-1 downto 0);
signal shiftRes : std_logic_vector(WORD_WIDTH-1 downto 0);
signal shiftC : std_logic;

-- logical operation signals
signal logicRes : std_logic_vector(WORD_WIDTH-1 downto 0);

begin

------------------------------------------------------------------------------
-- logic and basic shift operations
------------------------------------------------------------------------------
-- shift
shiftRes <= OpA(WORD_WIDTH downto 1);
shiftC   <= OpA(0);
-- logic
logicRes <= shiftRes                                                  when CTRL(1 downto 0)="00" else
            OpA(WORD_WIDTH-1 downto 0) or  OpB(WORD_WIDTH-1 downto 0) when CTRL(1 downto 0)="01" else  
            OpA(WORD_WIDTH-1 downto 0) and OpB(WORD_WIDTH-1 downto 0) when CTRL(1 downto 0)="10" else  
            OpA(WORD_WIDTH-1 downto 0) xor OpB(WORD_WIDTH-1 downto 0) when CTRL(1 downto 0)="11" else  
            (others=>'-');

------------------------------------------------------------------------------
-- Adding and comparing operations
------------------------------------------------------------------------------
uAddComp: process(OpA, OpB, OpC, CTRL(0))
variable auxRes : std_logic_vector(WORD_WIDTH+1 downto 0);
begin
    auxRes := ('0' & OpA) + ('0' & OpB) + OpC;
    if CTRL(0)='0' then  -- normal adding operations
        AddRes <= auxRes(WORD_WIDTH-1 downto 0);
        AddC   <= auxRes(WORD_WIDTH);
    else -- comparison operations
        AddRes <=  AuxRes(WORD_WIDTH) & auxRes(WORD_WIDTH-2 downto 0);
        AddC   <= '-';
    end if;
end process; 

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
Res <= AddRes      when CTRL="000" else  -- add
       AddRes      when CTRL="001" else  -- cmp
       MulRes      when CTRL="010" else  -- mul
       bshifterRes when CTRL="011" else  -- barrel shifter
       logicRes; -- simple shifter and logic

FlagC <= AddC      when CTRL="000" else  -- add
         shiftC; -- simple shift

end Behavioral;
