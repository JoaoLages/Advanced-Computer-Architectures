----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 10.02.2016 18:40:26
-- Design Name: 
-- Module Name: decoder - Behavioral
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
use IEEE.NUMERIC_STD.ALL;
use WORK.MAIN_DEFINITIONS.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity decoder is
  Port (
        clk     : in std_logic;
        reset   : in std_logic;
        -- instruction
        I       : in std_logic_vector(31 downto 0);
        PC      : in STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        -- register file connections
        RegOpA  : out STD_LOGIC_VECTOR(REG_ADD_WIDTH-1 downto 0);
        RegOpB  : out STD_LOGIC_VECTOR(REG_ADD_WIDTH-1 downto 0);
        RegOpD  : out STD_LOGIC_VECTOR(REG_ADD_WIDTH-1 downto 0);
        RegDA   : in STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);
        RegDB   : in STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);
        RegWE   : out STD_LOGIC;
        -- ALU connections
        ExCTRL  : out STD_LOGIC_VECTOR( 2 downto 0);
        ExOpA   : out STD_LOGIC_VECTOR(WORD_WIDTH downto 0);
        ExOpB   : out STD_LOGIC_VECTOR(WORD_WIDTH downto 0);
        ExOpC   : out STD_LOGIC;
        -- BR Control connections
        BrCond   : out STD_LOGIC_VECTOR( 2 downto 0);
        BrPC     : out STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        BrOffset : out STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        -- MEM connections
        MemCTRL  : out STD_LOGIC_VECTOR( 2 downto 0);
        -- Flag (MSR) conncections
        MSR_C_WE : out STD_LOGIC;
        MSR_C   : in STD_LOGIC
  );
end decoder;

architecture Behavioral of decoder is

signal IOpcode : std_logic_vector(5 downto 0);
signal IOpA, IOpB, IOpD : std_logic_vector(REG_ADD_WIDTH-1 downto 0);
signal IModifier : std_logic_vector(10 downto 0);

-- Immediate signals
signal Imm16, RImm16 : std_logic_vector(15 downto 0);
signal Imm32, ImmAux32 : std_logic_vector(31 downto 0);
signal MSR_I ,NewFlagKValue : std_logic;

begin

-- Get Instruction Fields
IOpcode   <= I(31 downto 26);
IOpA      <= I(20 downto 16);
IOpB      <= I(15 downto 11);
IOpD      <= I(25 downto 21);
IModifier <= I(10 downto 0);

-- define register file related signals
RegOpA    <= IOpA; 
RegOpB    <= IOpB; 
RegOpD    <= IOpD; 

-- define CONTROL related (i.e. branch) signals
BrCond <= I(23 downto 21) when std_match(IOpcode,"10-111") else
          "111"; -- unconditional operation

BrPC <= RegDB(PC_WIDTH-1 downto 0) when IOpcode="100110" and I(19)='1' else -- unconditional jump (i.e., with absolute address given by RB)
        Imm32(PC_WIDTH-1 downto 0) when IOpcode="101110" and I(19)='1' else -- unconditional jump (i.e., with absolute address given by Imm)
        PC;

BrOffset <= (PC_WIDTH-1 downto 0=>'0') when std_match(IOpcode,"10-110") and I(19)='1' else -- unconditional jump (i.e., with absolute address)
            RegDB(PC_WIDTH-1 downto 0) when std_match(IOpcode,"100110") and I(19)='0' else -- unconditional branch (i.e., with relative address given by RB)
            Imm32(PC_WIDTH-1 downto 0) when std_match(IOpcode,"101110") and I(19)='0' else -- unconditional branch (i.e., with relative address given by Imm32)
            RegDB(PC_WIDTH-1 downto 0) when std_match(IOpcode,"100111") else -- unconditional branch (i.e., with relative address given by RB)
            Imm32(PC_WIDTH-1 downto 0) when std_match(IOpcode,"101111") else -- unconditional branch (i.e., with relative address given by Imm32)
            std_logic_vector(to_unsigned(4, PC_WIDTH)); -- for all other cases, add 4 (i.e., sizeof(instruction))

-- define EXECUTION UNIT signals
ExCTRL   <= "000" when std_match(IOpcode,"00-0--") else                       -- A + B + C
            "000" when std_match(IOpcode,"0001--") and IModifier(0)='0' else  -- A + B + C
            "000" when std_match(IOpcode,"0011--") else  -- A + B + C
            "000" when std_match(IOpcode,"110---") and IModifier(0)='0' else  -- A + B + C
            "000" when std_match(IOpcode,"111---") else  -- A + B + C
            "001" when std_match(IOpcode,"0001--") and IModifier(0)='1' else  -- CMP(A,B)
            "010" when std_match(IOpcode,"01-000") else  -- MUL/MULH
            "011" when std_match(IOpcode,"01-001") else  -- barrel shifter 
            "100" when std_match(IOpcode,"100100") and IModifier(6 downto 5)/="11" else  -- shift right with carry in (ExC) and carry out (MSR_C) 
            "101" when std_match(IOpcode,"100100") and IModifier(6 downto 5) ="11" else  -- transfer (executed as ExOpA OR zero) 
            "101" when std_match(IOpcode,"10-110") and I(18) ='1' else  -- (*** NEW ***) the branch and link instruction must perform a transfer of PC value (executed as ExOpA OR zero) 
            "101" when std_match(IOpcode,"10-000") else  -- OR 
            "110" when std_match(IOpcode,"10-0-1") else  -- AND 
            "111" when std_match(IOpcode,"10-010") else  -- XOR 
            (others=>'-');

ExOpA   <= '0'                        & RegDA      when std_match(IOpcode,"00---0") else -- ADD/I, ADDC/I, ADDK/I, ADDKC/I,
           '0'                        & not RegDA  when std_match(IOpcode,"000--1") and IModifier(0) ='0' else -- RSUB, RSUBC, RSUBK, RSUBKC
           '0'                        & not RegDA  when std_match(IOpcode,"001--1") else -- RSUBI, RSUBCI, RSUBKI, RSUBKCI
           not RegDA(WORD_WIDTH-1)    & not RegDA  when std_match(IOpcode,"000101") and IModifier(1 downto 0) ="01" else -- CMP  (Operands A and B are signed)
           '1'                        & not RegDA  when std_match(IOpcode,"000101") and IModifier(1 downto 0) ="11" else -- CMPU (Operands A and B are unsigned)
           RegDA(WORD_WIDTH-1)        & RegDA      when std_match(IOpcode,"01-000") and IModifier(1 downto 0)/="11" else -- MUL, MULH, MULHUSU
           '0'                        & RegDA      when std_match(IOpcode,"01-000") and IModifier(1 downto 0) ="11" else -- MULHU
           '0'                        & RegDA      when std_match(IOpcode,"01-001") and IModifier(10)='1' else -- BSLL/I, BSLA/I
           '0'                        & RegDA      when std_match(IOpcode,"01-001") and IModifier(10 downto 9)="00" else -- BSRL/I
           RegDA(WORD_WIDTH-1)        & RegDA      when std_match(IOpcode,"01-001") and IModifier(10 downto 9)="01" else -- BSRA/I
           '-'                        & RegDA      when std_match(IOpcode,"10-0--") else                                 -- OR/I, AND/I, XOR/I, ANDN/I  
           RegDA(WORD_WIDTH-1)        & RegDA      when std_match(IOpcode,"100100") and IModifier(6 downto 5)="00" else  -- SRA
           MSR_C                      & RegDA      when std_match(IOpcode,"100100") and IModifier(6 downto 5)="01" else  -- SRC
           '0'                        & RegDA      when std_match(IOpcode,"100100") and IModifier(6 downto 5)="10" else  -- SRL
           (32 downto 8=>RegDA(7))    & RegDA( 7 downto 0) when std_match(IOpcode,"100100") and IModifier(6 downto 5)="11" and IModifier(0)='0' else -- SEXT8
           (32 downto 16=>RegDA(15))  & RegDA(15 downto 0) when std_match(IOpcode,"100100") and IModifier(6 downto 5)="11" and IModifier(0)='1' else -- SEXT16
           '0'                        & RegDA      when std_match(IOpcode,"11----") else -- LBU/I, LHU/I, LW/I, SB/I, SH/I, SW/I
           (WORD_WIDTH downto PC_WIDTH=>'0') & PC when std_match(IOpcode,"10-1--") else -- IMM, BR/I, BRL/I, BRA/I, BRAL/I, BEQ/I, BNE/I, BLT/I, BLE/I, BGT/I, BGE/I
           (others=>'-');

ExOpB   <= '0'                    & RegDB when std_match(IOpcode,"000---") and IModifier(0)='0' else -- ADD, RSUB, ADDC, RSUBC, ADDK, RSUBK, ADDKC, RSUBKC
           RegDB(WORD_WIDTH-1)    & RegDB when std_match(IOpcode,"000---") and IModifier(1 downto 0)="01" else -- CMP
           '0'                    & RegDB when std_match(IOpcode,"000---") and IModifier(1 downto 0)="11" else -- CMPU
           '0'                    & Imm32 when std_match(IOpcode,"001---") else                      -- ADDI, RSUBI, ADDIC, RSUBIC, ADDIK, RSUBIK, ADDIKC, RSUBIKC
           '0'                    & Imm32 when std_match(IOpcode,"--1---") else                      -- BSRAI, BSLAI, BSRLI, BSLLI, ORI, ANDI, XORI, ANDNI, LBUI, LHUI, LWI, SBI, SHI, SWI, BRI, BRLI, BRALI, BEWI, BNEI, BLTI, BLEI, BGTI, BGEI
           RegDB(WORD_WIDTH-1)    & RegDB when std_match(IOpcode,"010000") and IModifier(1)='0' else -- MUL, MULH
                              '0' & RegDB when std_match(IOpcode,"010000") and IModifier(1)='1' else -- MULHU, MULHSU
           IModifier(10 downto 9) & RegDB(30 downto 0) when std_match(IOpcode,"01-001") else         -- BSRA, BSLA, BSRL, BSLL
           '-'                    &     RegDB when std_match(IOpcode,"1000-0") else                  -- OR, XOR
           '-'                    &     RegDB when std_match(IOpcode,"100001") else                  -- AND
           '-'                    & not RegDB when std_match(IOpcode,"100011") else                  -- ANDN
           '0'                    & RegDB     when std_match(IOpcode,"11----") else                  -- LBU, LHU, LW, SB, SH, SW
           (others=>'0')                      when std_match(IOpcode,"10----") else                  -- SRA, SRC, SRL, SEXT8, SEXT16, BR, BRL, BRA, BRAL, BEQ, BNE, BLT, BLE, BGT, BGE (*** MODIFIED ***)
           (others=>'-');

ExOpC   <= '0'           when std_match(IOpcode,"00--00") else  -- ADD/I, ADDK/I 
           '1'           when std_match(IOpcode,"00--01") else  -- RSUB/I, RSUBK/I
           MSR_C         when std_match(IOpcode,"00--1-") else  -- ADDC/I, RSUBC/I, ADDKC/I, RSUBKC/I
           '0'           when std_match(IOpcode,"010000") and IModifier(1 downto 0) ="00" else  -- MUL  (low word)
           '1'           when std_match(IOpcode,"010000") and IModifier(1 downto 0)/="00" else  -- MULH (high word)
           IModifier(10) when std_match(IOpcode,"01-001") else  -- for barrel shifter instructions, specifies direction (0 - right; 1 - left)
           '0'           when std_match(IOpcode,"11----") else  -- LBU/I, LHU/I, LW/I, SB/I, SH/I, SW/I
           '-';

-- Memory related signals
MemCTRL <= "001" when std_match(IOpcode,"11-000") else -- read byte 
           "010" when std_match(IOpcode,"11-001") else -- read half-word
           "011" when std_match(IOpcode,"11-010") else -- read word
           "101" when std_match(IOpcode,"11-100") else -- write byte 
           "110" when std_match(IOpcode,"11-101") else -- write half-word
           "111" when std_match(IOpcode,"11-110") else -- write word
           "000"; -- no memory operation

-- Write-Back related signals
RegWE   <= '1' when std_match(IOpcode,"00----") else -- Arithmetic
           '1' when std_match(IOpcode,"01-0--") else -- MUL, BS
           '0' when std_match(IOpcode,"101100") else -- IMM
           '0' when std_match(IOpcode,"10-11-") else -- BR
           '1' when std_match(IOpcode,"11-0--") else -- LD
           '1' when std_match(IOpcode,"10-0--") else -- OR, AND, XOR ANDN
           '1' when std_match(IOpcode,"100100") else -- SRx, SEXT
           '1' when std_match(IOpcode,"10-110") and I(18) ='1' else -- BRL (*** NEW ***)
           '0';

MSR_C_WE <= '1' when std_match(IOpcode,"00-0--") else
            '1' when std_match(IOpcode,"100100") and IModifier(6 downto 5)/="11" else -- (*** MODIFIED ***)
            '0';

-- Immediate related signals
Imm16  <= I(15 downto 0);
RImm16 <= (others=>'0') when reset='1' else 
          Imm16 when rising_edge(clk) and IOpcode="101100";
ImmAux32 <= RImm16  & Imm16 when MSR_I='1' else
            (15 downto 0=>Imm16(15)) & Imm16;
Imm32  <= not ImmAux32 when std_match(IOpcode,"10-011") else ImmAux32;

NewFlagKValue <= '1' when Iopcode="101100" else '0';
MSR_I  <= NewFlagKValue when rising_edge(clk);

end Behavioral;
