-- Library declarations
library IEEE;
use IEEE.STD_LOGIC_1164.all;

package main_definitions is

-- Declare primary constants...
constant REG_ADD_WIDTH     : NATURAL := 5;
constant WORD_WIDTH        : NATURAL := 32;
constant INSTRUCTION_WIDTH : NATURAL := 32;
constant PC_WIDTH          : NATURAL := 16;

-- Other indirect constants...
constant N_REGISTERS       : NATURAL := 2**REG_ADD_WIDTH;

-- Declare types...
type word is array (WORD_WIDTH-1 downto 0) of std_logic;
type word_array is array (integer range <>) of STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);

-- declare components
component simpleCore
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
end component;

component decoder
  Port (
        clk     : in std_logic;
        reset : in STD_LOGIC;
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
end component;

component register_file
    Port ( 
       clk : in STD_LOGIC;
       reset : in STD_LOGIC;
       -- Output Port A 
       OpA   : in  STD_LOGIC_VECTOR  (REG_ADD_WIDTH-1 downto 0);
       DoutA : out STD_LOGIC_VECTOR  (WORD_WIDTH-1 downto 0);
       -- Output Port B
       OpB   : in  STD_LOGIC_VECTOR  (REG_ADD_WIDTH-1 downto 0);
       DoutB : out STD_LOGIC_VECTOR  (WORD_WIDTH-1 downto 0);
       -- input/output Port D
       WE    : in  STD_LOGIC;
       OpD   : in  STD_LOGIC_VECTOR (REG_ADD_WIDTH-1 downto 0);
       DinD  : in  STD_LOGIC_VECTOR  (WORD_WIDTH-1 downto 0);
       DoutD : out STD_LOGIC_VECTOR  (WORD_WIDTH-1 downto 0)
     );
end component;

component branch_control
  Port (
        PC       : in  STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        Cond     : in  STD_LOGIC_VECTOR( 2 downto 0);
        Offset   : in  STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0);
        CondWord : in  STD_LOGIC_VECTOR(WORD_WIDTH-1 downto 0);
        NextPC   : out STD_LOGIC_VECTOR(PC_WIDTH-1 downto 0)
       );
end component;

component alu
    Port ( 
       -- Control
       CTRL  : in STD_LOGIC_VECTOR  ( 2 downto 0);
       -- Operands 
       OpA   : in STD_LOGIC_VECTOR  (WORD_WIDTH downto 0);
       OpB   : in STD_LOGIC_VECTOR  (WORD_WIDTH downto 0);
       OpC   : in STD_LOGIC;
       -- Result
       Res   : out STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0);
       -- flags
       FlagC : out STD_LOGIC
     );
end component;

component DualPortMemory
    Port ( 
	        CLK    : in   STD_LOGIC;
	        Addr_A : in   STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
	        DO_A   : out  STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0);
	        WE_B   : in   STD_LOGIC;
	        Addr_B : in   STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
	        DI_B   : in   STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0);
	        DO_B   : out  STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0)
			  );
end component;

type instruction is array (INSTRUCTION_WIDTH-1 downto 0) of std_logic;

-- Declare functions and procedures...

end main_definitions;

package body main_definitions is

---- Example 1
--  function <function_name>  (signal <signal_name> : in <type_declaration>  ) 
--                                       return <type_declaration> is
--    variable <variable_name>     : <type_declaration>;
--  begin
--    <variable_name> := <signal_name> xor <signal_name>;
--    return <variable_name>; 
--  end <function_name>;

end main_definitions;
