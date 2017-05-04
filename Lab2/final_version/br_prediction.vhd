----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 10.02.2016 12:04:57
-- Design Name: 
-- Module Name: register_file - Behavioral
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
use WORK.MAIN_DEFINITIONS.ALL;
use IEEE.NUMERIC_STD.ALL;

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

entity br_prediction is
    Port ( 
			  reset: in std_logic;
			  clk : in std_logic;
			  id_br_pc : in STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0); -- is actually ID_PC too
			  EX_Br_PC : in STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
			  ID_BrOffset : in STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
			  Next_PC : out STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
			  PB_WE : in STD_LOGIC;
			  ID_Op_is_Branch: in STD_LOGIC;
			  Taken : in STD_LOGIC
			);
end br_prediction;

architecture Behavioral of br_prediction is

-- declare signals
signal RegisterTable : word_array(16-1 downto 0);
signal IF_Tag : std_logic_vector (PC_WIDTH-5 downto 0);
signal IF_Index : std_logic_vector (3 downto 0);
signal ID_Tag : std_logic_vector (PC_WIDTH-5 downto 0);
signal ID_Index : std_logic_vector (3 downto 0);
signal Ex_Index : std_logic_vector (3 downto 0);
signal Jump_addr : STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
signal ID_BR_PC_aux: std_logic_vector(PC_WIDTH-1 downto 0);
signal ID_BR_PC_real: std_logic_vector(PC_WIDTH-1 downto 0);
--file l_file: TEXT open write_mode is "br_pred.log";

begin
ID_BR_PC_aux <= (others=>'0') when reset='1' else 
		RegisterTable(conv_integer(IF_Index))(19 downto 4)	when --prediction taken
					(RegisterTable(conv_integer(IF_Index))(1)='1' and IF_Tag=RegisterTable(conv_integer(IF_Index))(31 downto 20)) else
		Id_Br_PC+std_logic_vector(to_unsigned(4, PC_WIDTH)); -- prediction not taken

ID_BR_PC_real <= (others => '0') when reset='1' else
				ID_BR_PC_aux when rising_edge(clk);		
				
Next_PC <= (others=>'0') when reset='1' else 
				ID_BR_PC_aux;
		
					
IF_Tag <= (others=>'0') when reset='1' else
		  Id_Br_PC(PC_WIDTH-5 downto 0);
IF_Index <= (others=>'0') when reset='1' else
		  Id_Br_PC(PC_WIDTH-1 downto PC_WIDTH-4);
		  
ID_Tag <= (others=>'0') when reset='1' else
		  IF_Tag when rising_edge(clk);
ID_Index <= (others=>'0') when reset='1' else
		  IF_Index when rising_edge(clk);

Ex_Index <= (others=>'0') when reset='1' else
		  ID_Index when rising_edge(clk);

Jump_addr <= (others=>'0') when reset='1' else ID_Br_PC_real+ID_BrOffset;





Unit: for i in 0 to 2**4 -1 generate
	process(CLK,reset)
		variable pred : integer := 0;
		variable aux : std_logic_vector(WORD_WIDTH-1 downto 0);
		variable my_line : line;
	begin
	   if reset='1' then
	       RegisterTable(i) <= (others=>'0');
		elsif rising_edge(clk) then
			if PB_WE='1' then -- instrucao de branch ta no EX
				if conv_integer(Ex_Index)=i and not(conv_integer(Ex_index) = conv_integer(Id_index)) then
					pred := conv_integer(RegisterTable(i)(1 downto 0));
					if Taken='1' then
						aux := RegisterTable(i);
						if(pred<3) then
							RegisterTable(i) <= aux+std_logic_vector(to_unsigned(1, WORD_WIDTH));
						end if;
					else
						if(pred>0) then
							RegisterTable(i) <= aux-std_logic_vector(to_unsigned(1, WORD_WIDTH));
						end if;
					end if;
				end if;
			end if;
			
			-- instruçaoo de branch ta no ID
			if Id_Op_is_Branch = '1' and PB_WE='0' then
				if not( --se nao houver um branch no EX e tiver sido mal previsto
								(conv_integer(Id_Br_PC_real-Ex_Br_PC) = 4 and Taken = '1') or 
								(not(conv_integer(Id_Br_PC_real-Ex_Br_PC) = 4) and Taken = '0') 
				) then
					
					if conv_integer(ID_Index)=i then
							if not(ID_Tag=RegisterTable(i)(31 downto 20)) then
								RegisterTable(i)(31 downto 20)<=ID_Tag;
								RegisterTable(i)(19 downto 4)<=Jump_addr;
								RegisterTable(i)(3 downto 1) <= (others=>'0');
								RegisterTable(i)(0) <= '1'; -- começa em weak not taken
							end if;
						end if;
				end if;
			end if;
		end if;
	end process;
end generate;

end Behavioral;
