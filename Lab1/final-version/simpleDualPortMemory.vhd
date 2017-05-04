----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 19.02.2016 19:26:44
-- Design Name: 
-- Module Name: simpleDualPortMemory - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use WORK.MAIN_DEFINITIONS.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity DualPortMemory is
    Port ( 
	        CLK    : in   STD_LOGIC;
	        Addr_A : in   STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
	        DO_A   : out  STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0);
	        WE_B   : in   STD_LOGIC;
	        Addr_B : in   STD_LOGIC_VECTOR (PC_WIDTH-1 downto 0);
	        DI_B   : in   STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0);
	        DO_B   : out  STD_LOGIC_VECTOR (WORD_WIDTH-1 downto 0)
			  );
end DualPortMemory;

architecture Behavioral of DualPortMemory is

-- declare a matrix of bits... the memory
type MEM_TYPE is array (0 to (2**(PC_WIDTH-2))-1) of std_logic_vector(WORD_WIDTH-1 downto 0);

constant InitValue0 : MEM_TYPE := (
        0 => "001000" & "00001" & "00000" & "11111" & "11111111111", -- ADDI    R1,R0,#-1      R1 <- (0)  + (-1)     = -1 (C=0)
        1 => "001001" & "00010" & "00001" & "11111" & "11111111101", -- RSUBI   R2,R1,#-3      R2 <- (-3) - (-1)     = -2 (C=0)
        2 => "000111" & "00011" & "00000" & "00010" & "00000000000", -- RSUBKC  R3,R0,R2       R3 <- (-2) + /(0) +C  = -3
        3 => "000011" & "00100" & "00000" & "00001" & "00000000000", -- RSUBC   R4,R0,R1       R4 <- (-1) + /(0) +C  = -2 (C=1)
        4 => "000010" & "00100" & "00100" & "00011" & "00000000000", -- ADDC    R4,R4,R3       R4 <- (-2) + (-3) +C  = -4 (C=1)
        5 => "000000" & "00101" & "00100" & "00001" & "00000000000", -- ADD     R5,R4,R1       R5 <- (-4) + (-1)     = -5 (C=1)
        6 => "000011" & "00110" & "00101" & "00001" & "00000000000", -- RSUBC   R6,R5,R1       R6 <- (-1) + /(-5)+C  = +4 (C=0)
        7 => "000001" & "00110" & "00110" & "00010" & "00000000000", -- RSUB    R6,R6,R2       R6 <- (-2) - (+4)     = -6 (C=1)
        8 => "001110" & "00111" & "00000" & "11111" & "11111111001", -- ADDIKC  R7,R0,#-7      R7 <- (0)  + (-7) +C  = -6
        9 => "001010" & "01000" & "00111" & "11111" & "11111111110", -- ADDIC   R8,R7,#-2      R8 <- (-6) + (-2) +C  = -7 (C=1)
        10 => "000101" & "01001" & "01000" & "00000" & "00000000001", -- CMP     R9,R8,R0       R9 <- (0)  - (-7)     = sign positive
        11 => "000101" & "01001" & "01000" & "00000" & "00000000011", -- CMPU    R9,R8,R0       R5 <- (0)  - (-7)     = sign negative
        12 => "101110" & "00000" & "00000" & "00000" & "00000100000", -- BRI     =>20           PC <-- PC + 8*4
        ---------------------------------------------------------------------------------------------------------------------------
        20 => "101100" & "00000" & "00000" & "01111" & "11111111111", -- IMM     x7FFF          IMM<-- x7FFF
        21 => "101000" & "00001" & "00000" & "11111" & "11111111111", -- ORI     R1,R0,#-1      R1 <-- x7FFFFFFF (sign positive)
        22 => "101100" & "00000" & "00000" & "10000" & "00000000000", -- IMM     x8000          IMM<-- x8000 
        23 => "001000" & "00010" & "00000" & "00000" & "00000000000", -- ADDI    R2,R0,#0       R2 <-- x80000000 (sign negative)
        24 => "101100" & "00000" & "00000" & "10000" & "00000000000", -- IMM     x8000          IMM<-- x8000 
        25 => "101011" & "00011" & "00001" & "11111" & "11111111111", -- ANDNI   R3,R1,#xFFFF   R3 <-- x7FFF0000 (sign positive)
        26 => "101100" & "00000" & "00000" & "01111" & "11111111111", -- IMM     x8000          IMM<-- x7FFF 
        27 => "101010" & "00100" & "00011" & "00000" & "00000000000", -- XORI    R4,R3,#xFFFF   R4 <-- x00000000 (sign positive)
        28 => "101111" & "00000" & "00100" & "00000" & "00000001000", -- BEQI    =>30           PC <-- PC + 2*4
        29 => "101110" & "00000" & "00000" & "11111" & "11111011100", -- BRI     =>20           PC <-- PC - 9*4
        ---------------------------------------------------------------------------------------------------------------------------
        30 => "001000" & "00001" & "00000" & "11111" & "11111111110", -- ADDI    R1,R0,#-2      R1 <-- -2
        31 => "010000" & "00010" & "00001" & "00001" & "00000000000", -- MUL     R2,R1,R1       R2 <-- +4
        32 => "010000" & "00011" & "00001" & "00001" & "00000000001", -- MULH    R3,R1,R1       R3 <-- 0
        33 => "010000" & "00100" & "00001" & "00001" & "00000000011", -- MULHU   R4,R1,R1       R4 <-- -4
        34 => "010000" & "00101" & "00010" & "00001" & "00000000010", -- MULHSU  R5,R2,R4       R5 <-- +3
        35 => "101110" & "00010" & "01100" & "00000" & "00010010100", -- BRALI   R2,==>37       R2 <-- 140, PC <-- 37x4
        36 => "100110" & "00000" & "01000" & "00000" & "00000000000", -- BRA     R0             PC <-- 0
        37 => "100111" & "00100" & "00001" & "00000" & "00000000000", -- BGT     R1,R0          PC <-- PC + 4
        38 => "100111" & "00101" & "00001" & "00000" & "00000000000", -- BGE     R1,R0          PC <-- PC + 4
        39 => "100111" & "00011" & "00010" & "00000" & "00000000000", -- BLE     R2,R0          PC <-- PC + 4
        ---------------------------------------------------------------------------------------------------------------------------
        40 => "001000" & "00001" & "00000" & "11111" & "11100000001", -- ADDI    R1,R0,#xFF01   R1 <-- 0xFFFFFF01
        41 => "100100" & "00010" & "00001" & "00000" & "00001100001", -- SEXT16  R2,R1          R2 <-- 0xFFFFFF01
        42 => "100100" & "00011" & "00010" & "00000" & "00001100000", -- SEXT8   R3,R2          R3 <-- 0x00000001
        43 => "100100" & "00100" & "00010" & "00000" & "00001000001", -- SRL     R4,R2          R4 <-- 0x7FFFFF80 (C=1)
        44 => "100100" & "00101" & "00010" & "00000" & "00001000001", -- SRL     R5,R2          R5 <-- 0x7FFFFF80 (C=1)
        45 => "100100" & "00110" & "00100" & "00000" & "00000100001", -- SRC     R6,R4          R6 <-- 0xBFFFFFC0 (C=0)
        46 => "100100" & "00111" & "00110" & "00000" & "00000100001", -- SRC     R7,R6          R7 <-- 0x5FFFFFE0 (C=0)
        47 => "100100" & "01000" & "00110" & "00000" & "00000000001", -- SRA     R8,R6          R8 <-- 0xDFFFFFE0 (C=0)
        48 => "100100" & "01001" & "00111" & "00000" & "00000000001", -- SRA     R9,R7          R9 <-- 0x2FFFFFF0 (C=0)
        49 => "000000" & "00000" & "00001" & "00001" & "00000000000", -- ADD     R0,R1,R1       R0 <-- 0 (NOP)
        ---------------------------------------------------------------------------------------------------------------------------
        50 => "101100" & "00000" & "00000" & "01111" & "11111111111", -- IMM     x7FFF          IMM<-- x7FFF
        51 => "101000" & "00001" & "00000" & "11110" & "00000001111", -- ORI     R1,R0,#xF00F   R1 <-- x7FFFF00F
        52 => "101000" & "00010" & "00000" & "00000" & "00110010000", -- ORI     R2,R0,#400     R2 <-- 400
        53 => "110100" & "00001" & "00010" & "00000" & "00000000000", -- SB      (R0+R2),R1     M[400] <-- 0x0F
        54 => "111100" & "00001" & "00010" & "00000" & "00000000101", -- SBI     5(R2),R1       M[405] <-- 0x0F
        55 => "111100" & "00001" & "00010" & "00000" & "00000001010", -- SBI     10(R2),R1      M[410] <-- 0x0F
        56 => "111100" & "00001" & "00010" & "00000" & "00000001111", -- SBI     15(R2),R1      M[415] <-- 0x0F
        57 => "111101" & "00001" & "00010" & "00000" & "00000000010", -- SHI     2(R2),R1       M[402] <-- 0xF00F
        58 => "111110" & "00001" & "00010" & "00000" & "00000010000", -- SWI     16(R2),R1      M[416] <-- 0x7FFFF00F
        59 => "111001" & "00011" & "00010" & "00000" & "00000000100", -- LHUI    R3,4(R2)       R3 <-- 0x00000F00
        60 => "111001" & "00100" & "00010" & "00000" & "00000001010", -- LHUI    R4,10(R2)      R4 <-- 0x0000000F
        61 => "110010" & "00101" & "00010" & "00000" & "00000000000", -- LW      R5,(R0+R2)     R5 <-- 0xF00F000F
        62 => "111010" & "00110" & "00010" & "00000" & "00000010000", -- LWI     R6,16(R2)      R6 <-- 0x7FFFF00F
        64 => "101000" & "00111" & "00000" & "00000" & "00100011000", -- ORI     R7,#280        R7 <-- 280
        65 => "100111" & "00100" & "00000" & "00111" & "00000000000", -- BGT     R0,R7          PC <-- PC + 4
        66 => "101110" & "00000" & "00000" & "00000" & "00000001000", -- BRI     8              PC <-- PC + 8
        68 => "100110" & "00001" & "01100" & "00111" & "00000000000", -- BRAL    R1,R7          PC <-- 70x4
        ---------------------------------------------------------------------------------------------------------------------------
        70 => "001000" & "00001" & "00000" & "00000" & "00000000001", -- ADDI    R1,R0,#1       R1 <-- 1
        71 => "011001" & "00010" & "00001" & "00000" & "10000000001", -- BSLLI   R2,R1,#1       R2 <-- 2
        72 => "011001" & "00011" & "00010" & "00000" & "11000000010", -- BSLAI   R3,R2,#2       R3 <-- 8
        73 => "011001" & "00100" & "00011" & "00000" & "11000011100", -- BSLLI   R4,R3,#16      R4 <-- 0x80000000
        74 => "011001" & "00101" & "00100" & "00000" & "01000011100", -- BSRAI   R5,R4,#16      R5 <-- 0xFFFFFFF8
        75 => "011001" & "00110" & "00101" & "00000" & "00000000001", -- BSRLI   R6,R5,#1       R6 <-- 0x7FFFFFFC
        76 => "011001" & "00111" & "00110" & "00000" & "01000011100", -- BSRAI   R5,R4,#16      R7 <-- 7
        77 => "101111" & "00001" & "00001" & "00000" & "00000000000", -- BNEI    R1,#0          PC <-- 77x4
        others => x"80000000" -- NOP
);

constant InitValue1 : MEM_TYPE := (
        --                        N=100; data = 0x80;
          --0 => "101000" & "00001" & "00000" & "00000" & "00000010100", -- ori     r1,r0,#20
          0 => "101000" & "00001" & "00000" & "00000" & "00001100100", -- ori     r1,r0,#100
          1 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
          2 => "101000" & "00010" & "00000" & "10000" & "00000000000", -- ori     r2,r0,#0x8000
        --                        r16 = fibonacci();
          3 => "101110" & "11111" & "01100" & "00000" & "10000000000", -- brali   r31,fibonacci
          4 => "100000" & "11110" & "00000" & "10000" & "00000000000", -- or      r30,r0,r16
        --****************************************************************************
        -- CHECK POINT #1: THE IMPLEMENTATION HAS AN ERROR IF IT REACHES 
        --                 THE FOLLOWING INSTRUCTION WITH ( r30 != f887 )
        --****************************************************************************
        --                        r16 = randomize();
          5 => "101110" & "11111" & "01100" & "00000" & "10010000000", -- brali   r31,randomize
          6 => "100000" & "11110" & "00000" & "10000" & "00000000000", -- or      r30,r0,r16
        --****************************************************************************
        -- CHECK POINT #2: THE IMPLEMENTATION HAS AN ERROR IF IT REACHES 
        --                 THE FOLLOWING INSTRUCTION WITH ( r30 != 6f9e )
        --****************************************************************************
        --                        r16 = sortUp();
          7 => "101110" & "11111" & "01100" & "00000" & "10100000000", -- brali   r31,sortUp
          8 => "100000" & "11110" & "00000" & "10000" & "00000000000", -- or      r30,r0,r16
        --****************************************************************************
        -- CHECK POINT #3: THE IMPLEMENTATION HAS AN ERROR IF IT REACHES 
        --                 THE FOLLOWING INSTRUCTION WITH ( r30 != 00570959 )
        --                 CORRESPONDING TO 87 (0x0057) SORTING LOOPS, OR OF A  
        --                 TOTAL OF 2393 SWAPS BETWEEN DATA ELEMENTS
        --****************************************************************************
        --                        while (1)
          9 => "100110" & "00000" & "00000" & "00000" & "00000000000", -- br      r0
        -------------------------------------------------------------------------------
        -- 0400h : int fibonacci(void)
        --              return value (checksum) in r16
        -------------------------------------------------------------------------------
        -- // r3 <- N | r4 <- base_address | r16 <- accumulator | r31 <- return address
        256 => "001000" & "11111" & "11111" & "00000" & "00000000100", -- addi    r31,r31,#4
        257 => "100000" & "00011" & "00000" & "00001" & "00000000000", -- or      r3,r0,r1
        258 => "100000" & "00100" & "00000" & "00010" & "00000000000", -- or      r4,r0,r2
        --                        Data[0] = 1; checksum=1;
        259 => "101000" & "00101" & "00000" & "00000" & "00000000001", -- ori     r5,r0,#1
        260 => "111110" & "00101" & "00100" & "00000" & "00000000000", -- swi     r5,0(r4)   M[r4+0] <- r5
        261 => "000000" & "10000" & "00000" & "00101" & "00000000000", -- add     r16,r0,r5
        --                        if (N>1) {Data[1] = 1; checksum+=1;}
        262 => "000001" & "00011" & "00101" & "00011" & "00000000000", -- rsub    r3,r5,r3
        263 => "101111" & "00000" & "00011" & "00000" & "00000111100", -- beqi    r3,_end_fibonacci
        264 => "111110" & "00101" & "00100" & "00000" & "00000000100", -- swi     r5,#4(r4)
        265 => "001000" & "00100" & "00100" & "00000" & "00000001000", -- addi    r4,r4,#8
        266 => "000000" & "10000" & "10000" & "00101" & "00000000000", -- add     r16,r16,r5
        -- _fibonacci_loop:
        --                        for (i=2;i<N; i++) {Data[i] = (Data[i-1]+Data[i-2]) & 0xffff; checksum += Data[i]; }
        267 => "001000" & "00011" & "00011" & "11111" & "11111111111", -- addi    r3,r3,#-1
        268 => "101111" & "00011" & "00011" & "00000" & "00000101000", -- blei    r3,_end_fibonacci
        269 => "111010" & "00110" & "00100" & "11111" & "11111111000", -- lwi     r6,#-8(r4)
        270 => "111010" & "00111" & "00100" & "11111" & "11111111100", -- lwi     r7,#-4(r4)
        271 => "000000" & "00101" & "00110" & "00111" & "00000000000", -- add     r5,r6,r7
        272 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
        273 => "101001" & "00101" & "00101" & "11111" & "11111111111", -- andi    r5,r5,#0xffff
        274 => "110110" & "00101" & "00100" & "00000" & "00000000000", -- sw      r5,r0(r4)
        275 => "000000" & "10000" & "10000" & "00101" & "00000000000", -- add     r16,r16,r5
        276 => "001000" & "00100" & "00100" & "00000" & "00000000100", -- addi    r4,r4,#4
        277 => "101110" & "00000" & "00000" & "11111" & "11111011000", -- bri     _fibonacci_loop
        -- _end_fibonacci:
        --                        return checksum & 0xffff;
        278 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
        279 => "101001" & "10000" & "10000" & "11111" & "11111111111", -- andi    r16,r16,#0xffff
        280 => "100110" & "00000" & "01000" & "11111" & "00000000000", -- bra     r31
        -------------------------------------------------------------------------------
        -- 0480h: int randomize(void)
        --             return value (checksum) in r16
        -------------------------------------------------------------------------------
            -- // r3 <- N | r4 <- base_address | r5 <- 1103515245 | r6 <- 12345 | r16 <- accumulator | r31 <- return address
        288 => "001000" & "11111" & "11111" & "00000" & "00000000100", -- addi    r31,r31,4
        289 => "100000" & "00011" & "00000" & "00001" & "00000000000", -- or      r3,r0,r1
        290 => "100000" & "00100" & "00000" & "00010" & "00000000000", -- or      r4,r0,r2
        291 => "101100" & "00000" & "00000" & "01000" & "00111000110", -- imm     #0x41c6
        292 => "101000" & "00101" & "00000" & "01001" & "11001101101", -- ori     r5,r0,#0x4e6d
        293 => "101000" & "00110" & "00000" & "00110" & "00000111001", -- ori     r6,r0,#0x3039
        294 => "100010" & "10000" & "10000" & "10000" & "00000000000", -- xor     r16,r16,r16
        --                        if (N<=0) return 0;
        295 => "101111" & "00011" & "00011" & "00000" & "00001010000", -- blei    r3,_end_randomize
        --                        for (i=0, checksum=0; aux=0;i<N;i++) {Data[i] = (Data[i]*1103515245 + 12345)>>16; checksum+=Data[i];}
        296 => "111010" & "10001" & "00100" & "00000" & "00000000000", -- lwi     r17,#0(r4)
        297 => "111010" & "10010" & "00100" & "00000" & "00000000100", -- lwi     r18,#4(r4)
        298 => "111010" & "10011" & "00100" & "00000" & "00000001000", -- lwi     r19,#8(r4)
        299 => "111010" & "10100" & "00100" & "00000" & "00000001100", -- lwi     r20,#12(r4)
        300 => "010000" & "10001" & "10001" & "00101" & "00000000000", -- mul     r17,r17,r5
        301 => "010000" & "10010" & "10010" & "00101" & "00000000000", -- mul     r18,r18,r5
        302 => "010000" & "10011" & "10011" & "00101" & "00000000000", -- mul     r19,r19,r5
        303 => "000000" & "10001" & "10001" & "00110" & "00000000000", -- add     r17,r17,r6
        304 => "000000" & "10010" & "10010" & "00110" & "00000000000", -- add     r18,r18,r6
        305 => "011001" & "10001" & "10001" & "00000" & "01000010000", -- bsrai   r17,r17,#16
        -- _loop_randomize:
        306 => "111110" & "10001" & "00100" & "00000" & "00000000000", -- swi     r17,#0(r4)
        307 => "000000" & "10000" & "10000" & "10001" & "00000000000", -- add     r16,r16,r17
        308 => "001000" & "00011" & "00011" & "11111" & "11111111111", -- addi    r3,r3,#-1
        309 => "001000" & "00100" & "00100" & "00000" & "00000000100", -- addi    r4,r4,#4
        310 => "011001" & "10001" & "10010" & "00000" & "01000010000", -- bsrai   r17,r18,#16
        311 => "000000" & "10010" & "10011" & "00110" & "00000000000", -- add     r18,r19,r6
        312 => "010000" & "10011" & "10100" & "00101" & "00000000000", -- mul     r19,r20,r5
        313 => "111010" & "10100" & "00100" & "00000" & "00000001100", -- lwi     r20,#12(r4)
        314 => "101111" & "00100" & "00011" & "11111" & "11111100000", -- bgti    r3,_loop_randomize
        -- _end_randomize:
        --                        return checksum & 0xffff;
        315 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
        316 => "101001" & "10000" & "10000" & "11111" & "11111111111", -- andi    r16,r16,#0xffff
        317 => "100110" & "00000" & "01000" & "11111" & "00000000000", -- bra     r31
        -------------------------------------------------------------------------------
        -- 0500h: int SortUp(void)
        --             return value (checksum) in r16
        -------------------------------------------------------------------------------
        -- // r3 <- N | r5 <- #swaps | r6 <- #iters | r7 <- finished? | r31 <- return address
        320 => "001000" & "11111" & "11111" & "00000" & "00000000100", -- addi    r31,r31,4
        321 => "100000" & "00101" & "00000" & "00000" & "00000000000", -- or      r5,r0,r0
        322 => "100000" & "00110" & "00000" & "00000" & "00000000000", -- or      r6,r0,r0
        --                        if (N<=1) {return 0} else {finished=0};
        323 => "001001" & "01000" & "00001" & "00000" & "00000000001", -- rsubi   r8,r1,#1
        324 => "101111" & "00101" & "01000" & "00000" & "00001000100", -- bgei    r8,_end_SortUp
        --                        finished=1; m = N-1; ptr = base_ptr;
        -- _loopAgain_sortUp
        325 => "101000" & "00111" & "00000" & "00000" & "00000000001", -- ori     r7,r0,#1
        326 => "001000" & "00011" & "00001" & "11111" & "11111111111", -- addi    r3,r1,#-1
        327 => "100000" & "00100" & "00000" & "00010" & "00000000000", -- or      r4,r0,r2
        --                        if (ptr[0]>ptr[1]) swap(ptr[0],ptr[1]);
        -- _next_SortUp:
        328 => "111010" & "10000" & "00100" & "00000" & "00000000000", -- lwi     r16,0(r4)
        329 => "111010" & "10001" & "00100" & "00000" & "00000000100", -- lwi     r17,4(r4)
        330 => "001000" & "00100" & "00100" & "00000" & "00000000100", -- addi    r4,r4,#4
        331 => "001000" & "00011" & "00011" & "11111" & "11111111111", -- addi    r3,r3,#-1
        332 => "000101" & "10010" & "10001" & "10000" & "00000000001", -- cmp     r18,r17,r16 -- r18>=0 if r16>r17 
        333 => "101111" & "00011" & "10010" & "00000" & "00000010100", -- blei    r18,_skip_SortUp
        334 => "111110" & "10001" & "00100" & "11111" & "11111111100", -- swi     r17,-4(r4) 
        335 => "111110" & "10000" & "00100" & "00000" & "00000000000", -- swi     r16,0(r4)
        336 => "100000" & "00111" & "00000" & "00000" & "00000000000", -- or      r7,r0,r0
        337 => "001000" & "00101" & "00101" & "00000" & "00000000001", -- addi    r5,r5,#1
        -- _skip_Sortup:
        338 => "101111" & "00100" & "00011" & "11111" & "11111011000", -- bgti    r3,_next_SortUp
        --339 => "101111" & "00011" & "00011" & "11111" & "11111100000", -- sw      r17,r0(r4)
        --                        if (!finished) goto loopagain_sortUp
        339 => "001000" & "00110" & "00110" & "00000" & "00000000001", -- addi    r6,r6,#1
        340 => "101111" & "00000" & "00111" & "11111" & "11111000100", -- beq     r7,_loopAgain_sortUp
        -- _end_SortUp:
        --                        return ((#iters)<<16) | (#swaps & 0xffff))
        341 => "011001" & "10000" & "00110" & "00000" & "10000010000", -- bslli   r16,r6,#16
        342 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
        343 => "101001" & "10001" & "00101" & "11111" & "11111111111", -- andi    r17,r5,0xffff
        344 => "100000" & "10000" & "10000" & "10001" & "00000000000", -- or      r16,r16,r17
        345 => "100110" & "00000" & "01000" & "11111" & "00000000000", -- bra     r31
        -------------------------------------------------------------------------------
        -- others
        -------------------------------------------------------------------------------
        others => x"80000000" -- NOP
);

constant InitValue2 : MEM_TYPE := (
        --                        N=100; data = 0x8000;
          0 => "101000" & "00001" & "00000" & "00000" & "00001100100", -- ori     r1,r0,#100
          1 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
          2 => "101000" & "00010" & "00000" & "10000" & "00000000000", -- ori     r2,r0,#0x8000
        --                        r16 = fibonacci();
          3 => "101110" & "11111" & "01100" & "00000" & "10000000000", -- brali   r31,fibonacci
          4 => "100000" & "11110" & "00000" & "10000" & "00000000000", -- or      r30,r0,r16
        --****************************************************************************
        -- CHECK POINT #1: THE IMPLEMENTATION HAS AN ERROR IF IT REACHES 
        --                 THE FOLLOWING INSTRUCTION WITH ( r30 != f887 )
        --****************************************************************************
        --                        r16 = randomize();
          5 => "101110" & "11111" & "01100" & "00000" & "10010000100", -- brali   r31,randomize
          6 => "100000" & "11110" & "00000" & "10000" & "00000000000", -- or      r30,r0,r16
        --****************************************************************************
        -- CHECK POINT #2: THE IMPLEMENTATION HAS AN ERROR IF IT REACHES 
        --                 THE FOLLOWING INSTRUCTION WITH ( r30 != 6f9e )
        --****************************************************************************
        --                        r16 = sortUp();
          7 => "101110" & "11111" & "01100" & "00000" & "10100000100", -- brali   r31,sortUp
          8 => "100000" & "11110" & "00000" & "10000" & "00000000000", -- or      r30,r0,r16
        --****************************************************************************
        -- CHECK POINT #3: THE IMPLEMENTATION HAS AN ERROR IF IT REACHES 
        --                 THE FOLLOWING INSTRUCTION WITH ( r30 != 00570959 )
        --                 CORRESPONDING TO 87 (0x0057) SORTING LOOPS, OR OF A  
        --                 TOTAL OF 2393 SWAPS BETWEEN DATA ELEMENTS
        --****************************************************************************
        --                        while (1)
          9 => "100110" & "00000" & "00000" & "00000" & "00000000000", -- br      r0
        -------------------------------------------------------------------------------
        -- 0400h : int fibonacci(void)
        --              return value (checksum) in r16
        -------------------------------------------------------------------------------
        -- // r3 <- N | r4 <- base_address | r16 <- accumulator | r31 <- return address
        256 => "001000" & "11111" & "11111" & "00000" & "00000000100", -- addi    r31,r31,#4
        257 => "100000" & "00011" & "00000" & "00001" & "00000000000", -- or      r3,r0,r1
        258 => "100000" & "00100" & "00000" & "00010" & "00000000000", -- or      r4,r0,r2
        --                        Data[0] = 1; checksum=1;
        259 => "101000" & "00101" & "00000" & "00000" & "00000000001", -- ori     r5,r0,#1
        260 => "111110" & "00101" & "00100" & "00000" & "00000000000", -- swi     r5,0(r4)
        261 => "000000" & "10000" & "00000" & "00101" & "00000000000", -- add     r16,r0,r5
        --                        if (N>1) {Data[1] = 1; checksum+=1;}
        262 => "000001" & "00011" & "00101" & "00011" & "00000000000", -- rsub    r3,r5,r3
        263 => "101111" & "00000" & "00011" & "00000" & "00000111100", -- beqi    r3,_end_fibonacci
        264 => "111110" & "00101" & "00100" & "00000" & "00000000100", -- swi     r5,#4(r4)
        265 => "001000" & "00100" & "00100" & "00000" & "00000001000", -- addi    r4,r4,#8
        266 => "000000" & "10000" & "10000" & "00101" & "00000000000", -- add     r16,r16,r5
        -- _fibonacci_loop:
        --                        for (i=2;i<N; i++) {Data[i] = (Data[i-1]+Data[i-2]) & 0xffff; checksum += Data[i]; }
        267 => "001000" & "00011" & "00011" & "11111" & "11111111111", -- addi    r3,r3,#-1
        268 => "101111" & "00011" & "00011" & "00000" & "00000101000", -- blei    r3,_end_fibonacci
        269 => "111010" & "00110" & "00100" & "11111" & "11111111000", -- lwi     r6,#-8(r4)
        270 => "111010" & "00111" & "00100" & "11111" & "11111111100", -- lwi     r7,#-4(r4)
        271 => "000000" & "00101" & "00110" & "00111" & "00000000000", -- add     r5,r6,r7
        272 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
        273 => "101001" & "00101" & "00101" & "11111" & "11111111111", -- andi    r5,r5,#0xffff
        274 => "110110" & "00101" & "00100" & "00000" & "00000000000", -- sw      r5,r0(r4)
        275 => "000000" & "10000" & "10000" & "00101" & "00000000000", -- add     r16,r16,r5
        276 => "101110" & "00000" & "10000" & "11111" & "11111011100", -- brid    _fibonacci_loop
        277 => "001000" & "00100" & "00100" & "00000" & "00000000100", -- addi    r4,r4,#4
        -- _end_fibonacci:
        --                        return checksum & 0xffff;
        278 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
        279 => "101001" & "10000" & "10000" & "11111" & "11111111111", -- andi    r16,r16,#0xffff
        280 => "101101" & "10000" & "11111" & "00000" & "00000000000", -- rtsd    r31,#0
        -------------------------------------------------------------------------------
        -- 0480h: int randomize(void)
        --             return value (checksum) in r16
        -------------------------------------------------------------------------------
            -- // r3 <- N | r4 <- base_address | r5 <- 1103515245 | r6 <- 12345 | r16 <- accumulator | r31 <- return address
        289 => "100000" & "00011" & "00000" & "00001" & "00000000000", -- or      r3,r0,r1
        290 => "100000" & "00100" & "00000" & "00010" & "00000000000", -- or      r4,r0,r2
        291 => "101100" & "00000" & "00000" & "01000" & "00111000110", -- imm     #0x41c6
        292 => "101000" & "00101" & "00000" & "01001" & "11001101101", -- ori     r5,r0,#0x4e6d
        293 => "101000" & "00110" & "00000" & "00110" & "00000111001", -- ori     r6,r0,#0x3039
        --                        if (N<=0) return 0;
        294 => "101111" & "10011" & "00011" & "00000" & "00001010100", -- bleid   r3,_end_randomize
        295 => "100010" & "10000" & "10000" & "10000" & "00000000000", -- xor     r16,r16,r16
        --                        for (i=0, checksum=0; aux=0;i<N;i++) {Data[i] = (Data[i]*1103515245 + 12345)>>16; checksum+=Data[i];}
        296 => "111010" & "10001" & "00100" & "00000" & "00000000000", -- lwi     r17,#0(r4)
        297 => "111010" & "10010" & "00100" & "00000" & "00000000100", -- lwi     r18,#4(r4)
        298 => "111010" & "10011" & "00100" & "00000" & "00000001000", -- lwi     r19,#8(r4)
        299 => "111010" & "10100" & "00100" & "00000" & "00000001100", -- lwi     r20,#12(r4)
        300 => "010000" & "10001" & "10001" & "00101" & "00000000000", -- mul     r17,r17,r5
        301 => "010000" & "10010" & "10010" & "00101" & "00000000000", -- mul     r18,r18,r5
        302 => "010000" & "10011" & "10011" & "00101" & "00000000000", -- mul     r19,r19,r5
        303 => "000000" & "10001" & "10001" & "00110" & "00000000000", -- add     r17,r17,r6
        304 => "000000" & "10010" & "10010" & "00110" & "00000000000", -- add     r18,r18,r6
        305 => "011001" & "10001" & "10001" & "00000" & "01000010000", -- bsrai   r17,r17,#16
        -- _loop_randomize:
        306 => "111110" & "10001" & "00100" & "00000" & "00000000000", -- swi     r17,#0(r4)
        307 => "000000" & "10000" & "10000" & "10001" & "00000000000", -- add     r16,r16,r17
        308 => "001000" & "00011" & "00011" & "11111" & "11111111111", -- addi    r3,r3,#-1
        309 => "001000" & "00100" & "00100" & "00000" & "00000000100", -- addi    r4,r4,#4
        310 => "011001" & "10001" & "10010" & "00000" & "01000010000", -- bsrai   r17,r18,#16
        311 => "000000" & "10010" & "10011" & "00110" & "00000000000", -- add     r18,r19,r6
        312 => "010000" & "10011" & "10100" & "00101" & "00000000000", -- mul     r19,r20,r5
        313 => "101111" & "10100" & "00011" & "11111" & "11111100100", -- bgtid   r3,_loop_randomize
        314 => "111010" & "10100" & "00100" & "00000" & "00000001100", -- lwi     r20,#12(r4)
        -- _end_randomize:
        --                        return checksum & 0xffff;
        315 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
        316 => "101001" & "10000" & "10000" & "11111" & "11111111111", -- andi    r16,r16,#0xffff
        317 => "101101" & "10000" & "11111" & "00000" & "00000000100", -- rtsd    r31,#4
        318 => "100000" & "00000" & "00000" & "00000" & "00000000000", -- nop
        319 => "100110" & "00000" & "00000" & "00000" & "00000000000", -- br      r0
        -------------------------------------------------------------------------------
        -- 0500h: int SortUp(void)
        --             return value (checksum) in r16
        -------------------------------------------------------------------------------
        -- // r3 <- N | r5 <- #swaps | r6 <- #iters | r7 <- finished? | r31 <- return address
        321 => "100000" & "00101" & "00000" & "00000" & "00000000000", -- or      r5,r0,r0
        322 => "100000" & "00110" & "00000" & "00000" & "00000000000", -- or      r6,r0,r0
        --                        if (N<=1) {return 0} else {finished=0};
        323 => "001001" & "01000" & "00001" & "00000" & "00000000001", -- rsubi   r8,r1,#1
        324 => "101111" & "00101" & "01000" & "00000" & "00001000100", -- bgei    r8,_end_SortUp
        --                        finished=1; m = N-1; ptr = base_ptr;
        -- _loopAgain_sortUp
        325 => "101000" & "00111" & "00000" & "00000" & "00000000001", -- ori     r7,r0,#1
        326 => "001000" & "00011" & "00001" & "11111" & "11111111111", -- addi    r3,r1,#-1
        327 => "100000" & "00100" & "00000" & "00010" & "00000000000", -- or      r4,r0,r2
        --                        if (ptr[0]>ptr[1]) swap(ptr[0],ptr[1]);
        -- _next_SortUp:
        328 => "111010" & "10000" & "00100" & "00000" & "00000000000", -- lwi     r16,0(r4)
        329 => "111010" & "10001" & "00100" & "00000" & "00000000100", -- lwi     r17,4(r4)
        330 => "001000" & "00100" & "00100" & "00000" & "00000000100", -- addi    r4,r4,#4
        331 => "001000" & "00011" & "00011" & "11111" & "11111111111", -- addi    r3,r3,#-1
        332 => "000101" & "10010" & "10001" & "10000" & "00000000001", -- cmp     r18,r17,r16 -- r18>=0 if r16>r17 
        333 => "101111" & "00011" & "10010" & "00000" & "00000010100", -- blei    r18,_skip_SortUp
        334 => "111110" & "10001" & "00100" & "11111" & "11111111100", -- swi     r17,-4(r4) 
        335 => "111110" & "10000" & "00100" & "00000" & "00000000000", -- swi     r16,0(r4)
        336 => "100000" & "00111" & "00000" & "00000" & "00000000000", -- or      r7,r0,r0
        337 => "001000" & "00101" & "00101" & "00000" & "00000000001", -- addi    r5,r5,#1
        -- _skip_Sortup:
        338 => "101111" & "00100" & "00011" & "11111" & "11111011000", -- bgti    r3,_next_SortUp
        --339 => "101111" & "00011" & "00011" & "11111" & "11111100000", -- sw      r17,r0(r4)
        --                        if (!finished) goto loopagain_sortUp
        339 => "001000" & "00110" & "00110" & "00000" & "00000000001", -- addi    r6,r6,#1
        340 => "101111" & "00000" & "00111" & "11111" & "11111000100", -- beq     r7,_loopAgain_sortUp
        -- _end_SortUp:
        --                        return ((#iters)<<16) | (#swaps & 0xffff))
        341 => "011001" & "10000" & "00110" & "00000" & "10000010000", -- bslli   r16,r6,#16
        342 => "101100" & "00000" & "00000" & "00000" & "00000000000", -- imm     #0
        343 => "101001" & "10001" & "00101" & "11111" & "11111111111", -- andi    r17,r5,0xffff
        344 => "101101" & "10000" & "11111" & "00000" & "00000000100", -- rtsd    r31,#4
        345 => "100000" & "10000" & "10000" & "10001" & "00000000000", -- or      r16,r16,r17
        346 => "100110" & "00000" & "00000" & "00000" & "00000000000", -- br      r0
        -------------------------------------------------------------------------------
        -- others
        -------------------------------------------------------------------------------
        others => x"80000000" -- NOP
);

shared variable RAM : MEM_TYPE := InitValue2;

begin

-----------------------------------------
-- PORT A (read-only)
-----------------------------------------
uRead: process (CLK)
begin
    if rising_edge(CLK) then
        DO_A <= RAM(conv_integer(Addr_A(PC_WIDTH-1 downto 2)));
    end if;
end process;

-----------------------------------------
-- PORT B
-----------------------------------------
process (CLK)
begin
    if falling_edge(CLK) then
        if WE_B='1' then
            RAM(conv_integer(Addr_B(PC_WIDTH-1 downto 2))) := DI_B;
        end if;
    end if;
	 if rising_edge(CLK) then
		DO_B <= RAM(conv_integer(Addr_B(PC_WIDTH-1 downto 2)));
	end if;
end process;
         
end Behavioral;