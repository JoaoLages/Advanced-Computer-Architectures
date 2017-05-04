----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 15.02.2016 20:09:27
-- Design Name: 
-- Module Name: simpleCore - Behavioral
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

entity simpleCore is
  Port (
        -- global control signals
        clk   : in  STD_LOGIC;
        reset : in  STD_LOGIC;
        -- instruction memory signals
        PC    : out STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        I     : in  STD_LOGIC_VECTOR(31 downto 0);
        -- data memory signals
        MemAddress    : out STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        MemWriteEnable : out STD_LOGIC; 
        MemWriteData  : out STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0); 
        MemReadData   : in  STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0)
       );
end simpleCore;

architecture Behavioral of simpleCore is

--------------------------------------------------------------------------------------------
-- stage control signals
--------------------------------------------------------------------------------------------
signal IF_STAGE_ENABLE : STD_LOGIC;
signal ID_STAGE_ENABLE : STD_LOGIC;
signal EX_STAGE_ENABLE : STD_LOGIC;
signal MEM_STAGE_ENABLE : STD_LOGIC;
signal WB_STAGE_ENABLE : STD_LOGIC;

--------------------------------------------------------------------------------------------
-- IF  stage signals
--------------------------------------------------------------------------------------------
signal IF_PC : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);

--------------------------------------------------------------------------------------------
-- ID  stage signals
--------------------------------------------------------------------------------------------
-- instruction & branch condition
signal ID_I : STD_LOGIC_VECTOR(31 downto 0);
signal ID_PC, ID_NextPC : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
signal ID_BrCond : STD_LOGIC_VECTOR( 2 downto 0);
signal ID_BrPC, ID_BrOffset : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);

-- operands
signal ID_OpA, ID_OpB, ID_OpD_addr : STD_LOGIC_VECTOR(REG_ADD_WIDTH-1 downto 0);
signal ID_OpD : STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);
signal ID_DA, ID_DB, ID_DD, ID_RegDA, ID_RegDB, ID_RegDD : STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);

-- EX control & operands
signal ID_ExCTRL  : STD_LOGIC_VECTOR( 2 downto 0);
signal ID_ExOpA, ID_ExOpB : STD_LOGIC_VECTOR(WORD_WIDTH downto 0);
signal ID_ExOpC : STD_LOGIC;
signal ID_MSR_C_WE : STD_LOGIC;
signal ID_MSR_C_WE_dec : STD_LOGIC;
signal ID_MSR_C : STD_LOGIC;
-- EX memory
signal ID_MemCTRL  : STD_LOGIC_VECTOR( 2 downto 0);
signal Ex_OpD_addr : STD_LOGIC_VECTOR(REG_ADD_WIDTH-1 downto 0);
-- MEM
signal Mem_OpD : STD_LOGIC_VECTOR(REG_ADD_WIDTH-1 downto 0);
-- WB
signal ID_RegWE : STD_LOGIC;
signal ID_RegWE_dec : STD_LOGIC;
signal WB_OpD, Aux_OpD : STD_LOGIC_VECTOR(REG_ADD_WIDTH-1 downto 0);

--------------------------------------------------------------------------------------------
-- EX  stage signals
--------------------------------------------------------------------------------------------
-- EX control & operands
signal EX_CTRL  : STD_LOGIC_VECTOR( 2 downto 0);
signal Ex_OpA, Ex_OpB : STD_LOGIC_VECTOR(WORD_WIDTH downto 0);
signal Ex_OpC : STD_LOGIC;
signal EX_OpD : STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);
signal EX_FlagC, EX_MSR_C_WE, EX_MSR_C : STD_LOGIC;
signal EX_Result : STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);
signal EX_I : STD_LOGIC_VECTOR(31 downto 0);
signal EX_NextPC : STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);

-- EX control & operands
signal EX_MemCTRL  : STD_LOGIC_VECTOR( 2 downto 0);

-- WB
signal EX_RegWE : STD_LOGIC;

--------------------------------------------------------------------------------------------
-- MEM stage signals
--------------------------------------------------------------------------------------------
signal MEM_CTRL  : STD_LOGIC_VECTOR( 2 downto 0);
signal MEM_I : STD_LOGIC_VECTOR(31 downto 0);
signal MEM_ExResult, MEM_DataIn : STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);

-- WB
signal MEM_RegWE : STD_LOGIC;

--------------------------------------------------------------------------------------------
-- WB  stage signals
--------------------------------------------------------------------------------------------
signal WB_RegWE, RegWE : STD_LOGIC;
signal WB_I : STD_LOGIC_VECTOR(31 downto 0);
signal WB_MemCTRL : STD_LOGIC_VECTOR( 2 downto 0);
signal WB_RegDin, WB_ExResult, WB_StoreData: STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);

begin


--------------------------------------------------------------------------------------------
-- stage control signals
--------------------------------------------------------------------------------------------
-- activate one stage at a time // stall on specific conditions: 
--												-> if destination register of EX stage or Mem stage operation 
--													is one of the source registers of the ID operation, stall.


IF_STAGE_ENABLE  <= '0' when (ID_OpA = Ex_OpD_addr and (
	std_match(Ex_I(31 downto 26),"11-000") or
	std_match(Ex_I(31 downto 26),"11-001") or
	std_match(Ex_I(31 downto 26),"11-010") )) or (ID_OpA = Mem_OpD and (
	std_match(Mem_I(31 downto 26),"11-000") or
	std_match(Mem_I(31 downto 26),"11-001") or
	std_match(Mem_I(31 downto 26),"11-010") )) or (ID_OpB = Ex_OpD_addr and (
	std_match(Ex_I(31 downto 26),"11-000") or
	std_match(Ex_I(31 downto 26),"11-001") or
	std_match(Ex_I(31 downto 26),"11-010") )) or (ID_OpB = Mem_OpD and (
	std_match(Mem_I(31 downto 26),"11-000") or
	std_match(Mem_I(31 downto 26),"11-001") or
	std_match(Mem_I(31 downto 26),"11-010") )) or 
	(std_match(Ex_I(31 downto 26),"11-1--") and std_match(ID_I(31 downto 26),"11-0--") ) -- Store followed by Load operation not supported (needs 1 stall)
else '1';

ID_MSR_C_WE <= ID_MSR_C_WE_dec and ID_STAGE_ENABLE;
ID_RegWE <= ID_RegWE_Dec and ID_STAGE_ENABLE;

ID_STAGE_ENABLE  <= '0' when reset='1' or IF_STAGE_ENABLE='0'
	or (ID_OpA = Ex_OpD_addr and (
		std_match(Ex_I(31 downto 26),"11-000") or
		std_match(Ex_I(31 downto 26),"11-001") or
		std_match(Ex_I(31 downto 26),"11-010") )) 
	or (ID_OpA = Mem_OpD and (
		std_match(Mem_I(31 downto 26),"11-000") or
		std_match(Mem_I(31 downto 26),"11-001") or
		std_match(Mem_I(31 downto 26),"11-010") )) 
	or (ID_OpB = Ex_OpD_addr and (
		std_match(Ex_I(31 downto 26),"11-000") or
		std_match(Ex_I(31 downto 26),"11-001") or
		std_match(Ex_I(31 downto 26),"11-010") )) 
	or (ID_OpB = Mem_OpD and (
		std_match(Mem_I(31 downto 26),"11-000") or
		std_match(Mem_I(31 downto 26),"11-001") or
		std_match(Mem_I(31 downto 26),"11-010") )) 
	or (std_match(Ex_I(31 downto 26),"11-1--") and std_match(ID_I(31 downto 26),"11-0--")) -- Store followed by Load operation not supported (needs 1 stall)
else '1';

-- the 'or' operations added here are to prevent this stages from stalling when the previous ones stall
EX_STAGE_ENABLE  <= '0' when reset='1' else (ID_STAGE_ENABLE or EX_STAGE_ENABLE) when rising_edge(clk);

MEM_STAGE_ENABLE <= '0' when reset='1' else (EX_STAGE_ENABLE or MEM_STAGE_ENABLE) when rising_edge(clk);

WB_STAGE_ENABLE  <= '0' when reset='1' else (MEM_STAGE_ENABLE or WB_STAGE_ENABLE) when rising_edge(clk);

---------------------------------------------------------------------------------------------------------------
-- PC REGISTER
---------------------------------------------------------------------------------------------------------------

IF_PC <= (PC_WIDTH-1 downto 0=>'0') when reset='1' else
		 EX_NextPC when std_match(EX_I(31 downto 26),"10-11-") and EX_I(28 downto 26)="111" and EX_I(25)='1' and EX_NextPC/=ID_PC and IF_STAGE_ENABLE='1'  else -- BR delayed
		 EX_NextPC when std_match(EX_I(31 downto 26),"10-11-") and EX_I(28 downto 26)/="111" and EX_I(20)='1' and EX_NextPC/=ID_PC and IF_STAGE_ENABLE='1'  else -- BR delayed
		 EX_NextPC when EX_I(31 downto 26)="101101" and IF_STAGE_ENABLE='1'  else -- BR delayed (RTSD operation)
		 ID_NextPC when std_match(ID_I(31 downto 26),"10-11-") and ID_I(28 downto 26)="111" and ID_I(25)='0' and IF_STAGE_ENABLE='1'  else -- BR not delayed
		 ID_NextPC when std_match(ID_I(31 downto 26),"10-11-") and ID_I(28 downto 26)/="111" and ID_I(20)='0' and IF_STAGE_ENABLE='1'  else -- BR not delayed
       ID_PC+std_logic_vector(to_unsigned(4, PC_WIDTH)) when IF_STAGE_ENABLE='1' else -- No branches
		 ID_PC; -- Stall
		 
PC   <= IF_PC;

---------------------------------------------------------------------------------------------------------------
-- IF to ID stage registers
---------------------------------------------------------------------------------------------------------------
ID_PC <= (1 downto 0 => '0', others=>'1') when reset='1' else IF_PC when rising_edge(clk) and IF_STAGE_ENABLE='1'; --init ID_PC at -4, so that IF_PC inits at 0

---------------------------------------------------------------------------------------------------------------
-- DECODER UNIT
---------------------------------------------------------------------------------------------------------------
ID_I <= I; -- assuming memory has syncrounous read

uDecoder : decoder port map(
    clk      => clk,               reset  => reset,
    -- instruction
    I        => ID_I,              PC     => ID_PC,
    -- register file connections
    RegOpA   => ID_OpA,            RegOpB => ID_OpB,           RegOpD   => ID_OpD_addr,
    RegDA    => ID_DA,          RegDB  => ID_DB,
    RegWE    => ID_RegWE_dec,
    -- ALU connections
    ExCTRL   => ID_ExCTRL,
    ExOpA    => ID_ExOpA,          ExOpB  => ID_ExOpB,         ExOpC    => ID_ExOpC,
    -- BR connections
    BrCond   => ID_BrCond,         BrPC   => ID_BrPC,        BrOffset => ID_BrOffset,
    -- MEM connections
    MemCTRL  => ID_MemCTRL,
    -- Other/Flag connections
    MSR_C_WE => ID_MSR_C_WE_dec,       MSR_C => ID_MSR_C
);
	
-----------------------------------------------------------------------------------
-- Data Forwarding
-----------------------------------------------------------------------------------

-- Forwarding MSR_C: if operation in EX stage writes in MSR_C and operation in ID uses MSR_C
ID_MSR_C <= EX_FlagC when EX_MSR_C_WE = '1'
	else EX_MSR_C;

-- Forwarding Operand A (entrance of the decoder): 
ID_DA <= EX_Result when ID_OpA = Ex_OpD_addr and unsigned(Ex_OpD_addr) /= 0 and ( -- Don't forward when a NOP was introduced due to memory conflict
		(std_match(Ex_I(31 downto 26), "10-110") and Ex_I(18) = '1') or -- branch and link
		std_match(Ex_I(31 downto 26), "00----") or 
		std_match(Ex_I(31 downto 26), "01----") or
		std_match(Ex_I(31 downto 26), "1000--") or
		std_match(Ex_I(31 downto 26), "100100") or
		std_match(Ex_I(31 downto 26), "1010--")   )
	else MEM_ExResult when ID_OpA = Mem_OpD and (
	  (std_match(MEM_I(31 downto 26), "10-110") and MEM_I(18) = '1') or -- branch and link
		std_match(MEM_I(31 downto 26), "00----") or
		std_match(MEM_I(31 downto 26), "01----") or
		std_match(MEM_I(31 downto 26), "1000--") or
		std_match(MEM_I(31 downto 26), "100100") or
		std_match(MEM_I(31 downto 26), "1010--")	)	
	else ID_RegDA;


-- Forwarding Operand B(entrance of the decoder): 
ID_DB <= EX_Result when ID_OpB = Ex_OpD_addr and unsigned(Ex_OpD_addr) /= 0 and( -- Don't forward when a NOP was introduced due to memory conflict
	  (std_match(Ex_I(31 downto 26), "10-110") and Ex_I(18) = '1') or -- branch and link
		std_match(Ex_I(31 downto 26), "00----") or 
		std_match(Ex_I(31 downto 26), "01----") or
		std_match(Ex_I(31 downto 26), "1000--") or
		std_match(Ex_I(31 downto 26), "100100") or
		std_match(Ex_I(31 downto 26), "1010--")   )
	else MEM_ExResult when ID_OpB = Mem_OpD and (
	  (std_match(MEM_I(31 downto 26), "10-110") and MEM_I(18) = '1') or -- branch and link
		std_match(MEM_I(31 downto 26), "00----") or
		std_match(MEM_I(31 downto 26), "01----") or
		std_match(MEM_I(31 downto 26), "1000--") or
		std_match(MEM_I(31 downto 26), "100100") or
		std_match(MEM_I(31 downto 26), "1010--")	)	
	else ID_RegDB;

-- Forwarding Operand D
	ID_OpD <= EX_Result when ID_OpD_addr = Ex_OpD_addr and unsigned(Ex_OpD_addr) /= 0 and( -- Don't forward when a NOP was introduced due to memory conflict
	  (std_match(Ex_I(31 downto 26), "10-110") and Ex_I(18) = '1') or -- branch and link
		std_match(Ex_I(31 downto 26), "00----") or 
		std_match(Ex_I(31 downto 26), "01----") or
		std_match(Ex_I(31 downto 26), "1000--") or
		std_match(Ex_I(31 downto 26), "100100") or
		std_match(Ex_I(31 downto 26), "1010--")   )
	else MEM_ExResult when ID_OpD_addr = Mem_OpD and (
	  (std_match(MEM_I(31 downto 26), "10-110") and MEM_I(18) = '1') or -- branch and link
		std_match(MEM_I(31 downto 26), "00----") or
		std_match(MEM_I(31 downto 26), "01----") or
		std_match(MEM_I(31 downto 26), "1000--") or
		std_match(MEM_I(31 downto 26), "100100") or
		std_match(MEM_I(31 downto 26), "1010--")	)	
	else ID_RegDD;

---------------------------------------------------------------------------------------------------------------
-- REGISTER FILE
---------------------------------------------------------------------------------------------------------------
uRF : register_file port map(
    clk   => clk,           reset => reset,
    OpA   => ID_OpA,          OpB => ID_OpB,          OpD => Aux_OpD,     -- port addressing
    DoutA => ID_RegDA,      DoutB => ID_RegDB,      DoutD => ID_RegDD, -- port output
    WE    => RegWE,         DinD  => WB_RegDin                         -- RF write (port D only)
);

Aux_Opd <= WB_OpD when WB_RegWE = '1' and clk='1' else ID_OpD_addr;
---------------------------------------------------------------------------------------------------------------
-- BRANCH (i.e. PC) CONTROL
---------------------------------------------------------------------------------------------------------------
uBranchCTRL: branch_control port map (
    PC       => ID_BrPC,
    Offset   => ID_BrOffset,
    Cond     => ID_BrCond,
    CondWord => ID_DA,
    NextPC   => ID_NextPC
);

---------------------------------------------------------------------------------------------------------------
-- ID/OF to EX stage registers
---------------------------------------------------------------------------------------------------------------
EX_I 			<= (others=>'0') when reset='1' else ID_I			 when rising_edge(clk) and ID_STAGE_ENABLE='1' else (others=>'0') when ID_STAGE_ENABLE='0' and rising_edge(clk);
EX_CTRL     <= (others=>'0') when reset='1' else ID_ExCTRL   when rising_edge(clk) and ID_STAGE_ENABLE='1';
Ex_OpA      <= (others=>'0') when reset='1' else ID_ExOpA    when rising_edge(clk) and ID_STAGE_ENABLE='1';
Ex_OpB      <= (others=>'0') when reset='1' else ID_ExOpB    when rising_edge(clk) and ID_STAGE_ENABLE='1';
Ex_OpC      <=          '0'  when reset='1' else ID_ExOpC    when rising_edge(clk) and ID_STAGE_ENABLE='1';
EX_OpD      <= (others=>'0') when reset='1' else ID_OpD    when rising_edge(clk) and ID_STAGE_ENABLE='1';
EX_MSR_C_WE <=          '0'  when reset='1' or ID_STAGE_ENABLE='0'
								else ID_MSR_C_WE when rising_edge(clk) and ID_STAGE_ENABLE='1';
EX_MemCTRL  <= (others=>'0') when reset='1' else ID_MemCTRL  when rising_edge(clk) and ID_STAGE_ENABLE='1' else (others=>'0') when rising_edge(clk) and ID_STAGE_ENABLE='0';  
EX_RegWE    <=          '0'  when reset='1' else ID_RegWE  when rising_edge(clk) and ID_STAGE_ENABLE='1'; 
EX_OpD_addr <= (others=>'0') when reset='1' else ID_OpD_addr		 when rising_edge(clk) and ID_STAGE_ENABLE='1' else (others=>'0') when rising_edge(clk) and ID_STAGE_ENABLE='0';
EX_MSR_C    <= '0' when reset='1' else EX_FlagC when rising_edge(clk) and EX_MSR_C_WE='1' and EX_STAGE_ENABLE='1';
EX_NextPC 	<= (others=>'0') when reset='1' else ID_NextPC when rising_edge(clk) and ID_STAGE_ENABLE='1';

---------------------------------------------------------------------------------------------------------------
-- ALU
---------------------------------------------------------------------------------------------------------------
uALU: alu port map (
    CTRL => EX_CTRL,
    OpA  => Ex_OpA,          OpB   => Ex_OpB,           OpC => Ex_OpC,
    Res  => EX_Result,       FlagC => EX_FlagC
);


---------------------------------------------------------------------------------------------------------------
-- EX to MEM stage registers
---------------------------------------------------------------------------------------------------------------
MEM_CTRL     <= (others=>'0') when reset='1' else EX_MemCTRL when rising_edge(clk) and EX_STAGE_ENABLE='1';
MEM_ExResult <= (others=>'0') when reset='1' else EX_Result  when rising_edge(clk) and EX_STAGE_ENABLE='1';
MEM_DataIn   <= (others=>'0') when reset='1' else EX_OpD     when rising_edge(clk) and EX_STAGE_ENABLE='1';
MEM_RegWE    <=          '0'  when reset='1' else EX_RegWE   when rising_edge(clk) and EX_STAGE_ENABLE='1'; 
MEM_Opd		 <= (others=>'0') when reset='1' else Ex_Opd_Addr when rising_edge(clk) and EX_STAGE_ENABLE='1'; 
MEM_I			 <= (others=>'0') when reset='1' else Ex_I 		 when rising_edge(clk) and EX_STAGE_ENABLE='1'; 
---------------------------------------------------------------------------------------------------------------
-- DATA MEMORY
---------------------------------------------------------------------------------------------------------------
MemAddress   <= WB_ExResult(PC_WIDTH-1 downto 0) when std_match(WB_I(31 downto 26),"11-1--") else -- for Store Operations
				MEM_ExResult(PC_WIDTH-1 downto 0); 

MemWriteData <= MemReadData(31 downto  8) & WB_StoreData( 7 downto 0)                            when WB_MemCTRL="101" and MEM_ExResult(1 downto 0)="00" else -- write on byte 0
                MemReadData(31 downto 16) & WB_StoreData( 7 downto 0) & MemReadData( 7 downto 0) when WB_MemCTRL="101" and MEM_ExResult(1 downto 0)="01" else -- write on byte 1
                MemReadData(31 downto 24) & WB_StoreData( 7 downto 0) & MemReadData(15 downto 0) when WB_MemCTRL="101" and MEM_ExResult(1 downto 0)="10" else -- write on byte 2
                                            WB_StoreData( 7 downto 0) & MemReadData(23 downto 0) when WB_MemCTRL="101" and MEM_ExResult(1 downto 0)="11" else -- write on byte 3
                MemReadData(31 downto 16) & WB_StoreData(15 downto 0)                            when WB_MemCTRL="110" and MEM_ExResult(1)='0' else -- write on half-word 0
                                            WB_StoreData(15 downto 0) & MemReadData(15 downto 0) when WB_MemCTRL="110" and MEM_ExResult(1)='1' else -- write on half-word 1
                WB_StoreData; -- write word

-- with the current implementation, during the MEM Stage, only read operations are allowed
-- all writes are performed during the WB stage
MemWriteEnable <= '1' when WB_STAGE_ENABLE='1' and WB_MemCTRL(2)='1' else '0'; 

---------------------------------------------------------------------------------------------------------------
-- MEM to WB stage registers
---------------------------------------------------------------------------------------------------------------
WB_ExResult    <= (others=>'0') when reset='1' else MEM_ExResult when rising_edge(clk) and MEM_STAGE_ENABLE='1';
WB_StoreData   <= (others=>'0') when reset='1' else MEM_DataIn   when rising_edge(clk) and MEM_STAGE_ENABLE='1';
WB_MemCTRL     <= (others=>'0') when reset='1' else MEM_CTRL     when rising_edge(clk) and MEM_STAGE_ENABLE='1';
WB_ExResult    <= (others=>'0') when reset='1' else MEM_ExResult when rising_edge(clk) and MEM_STAGE_ENABLE='1';
WB_RegWE       <=          '0'  when reset='1' else MEM_RegWE    when rising_edge(clk) and MEM_STAGE_ENABLE='1'; 
WB_Opd			<= (others=>'0') when reset='1' else Mem_Opd 	  when rising_edge(clk) and MEM_STAGE_ENABLE='1'; 
WB_I			<= (others=>'0') when reset='1' else MEM_I 		 when rising_edge(clk) and MEM_STAGE_ENABLE='1'; 
---------------------------------------------------------------------------------------------------------------
-- WRITE BACK STAGE
---------------------------------------------------------------------------------------------------------------
WB_RegDin <= (WORD_WIDTH-1 downto  8=>'0') & MemReadData( 7 downto  0) when WB_MemCTRL="001" and WB_ExResult(1 downto 0)="00" else -- read from byte 0
             (WORD_WIDTH-1 downto  8=>'0') & MemReadData(15 downto  8) when WB_MemCTRL="001" and WB_ExResult(1 downto 0)="01" else -- read from byte 1
             (WORD_WIDTH-1 downto  8=>'0') & MemReadData(23 downto 16) when WB_MemCTRL="001" and WB_ExResult(1 downto 0)="10" else -- read from byte 2
             (WORD_WIDTH-1 downto  8=>'0') & MemReadData(31 downto 24) when WB_MemCTRL="001" and WB_ExResult(1 downto 0)="11" else -- read from byte 3
             (WORD_WIDTH-1 downto 16=>'0') & MemReadData(15 downto  0) when WB_MemCTRL="010" and WB_ExResult(1)='0'           else -- read from half-word 0
             (WORD_WIDTH-1 downto 16=>'0') & MemReadData(31 downto 16) when WB_MemCTRL="010" and WB_ExResult(1)='1'           else -- read from half-word 1
             MemReadData                                               when WB_MemCTRL="011"                                  else -- read word
             WB_ExResult; -- remaining operations

RegWE <= WB_RegWE when WB_STAGE_ENABLE='1' else '0';

end Behavioral;