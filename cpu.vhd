-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2017 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Pavel Janko (xjanko10)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni z pameti (DATA_RDWR='0') / zapis do pameti (DATA_RDWR='1')
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA obsahuje stisknuty znak klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna pokud IN_VLD='1'
   IN_REQ    : out std_logic;                     -- pozadavek na vstup dat z klavesnice
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- pokud OUT_BUSY='1', LCD je zaneprazdnen, nelze zapisovat,  OUT_WE musi byt '0'
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

	-- Stavy konecneho automatu
	type state is (S_FETCH, S_DECODE, S_PTR_INC, S_PTR_DEC, S_VAL_INC, S_VAL_INC_2, S_VAL_DEC, S_VAL_DEC_2,
		S_WHILE_BEGIN, S_WHILE_BEGIN_2, S_WHILE_BEGIN_3, S_WHILE_BEGIN_4, S_WHILE_END, S_WHILE_END_2,
		S_WHILE_END_3, S_WHILE_END_4, S_WHILE_END_5, S_PUTCHAR, S_PUTCHAR_2, S_GETCHAR, S_GETCHAR_2, S_BREAK,
		S_BREAK_2, S_BREAK_3, S_END, S_COMMENT);

	signal present_state, next_state : state;

	-- Signaly pro jednotlive registry
	signal pc_address : std_logic_vector(11 downto 0); -- Velikost CODE_ADDR
	signal ptr_address : std_logic_vector(9 downto 0); -- Velikost DATA_ADDR
	signal cnt_reg : std_logic_vector(7 downto 0); -- Velikost 8 bitu

	-- Signaly pro inkrementaci a dekrementaci registru,
	-- pripadne nastaveni na hodnotu 1
	signal pc_inc, pc_dec, ptr_inc, ptr_dec, cnt_reset, cnt_inc, cnt_dec : std_logic;

begin

	-- Vsechny registry funguji na podobnem principu,
	-- inspirace podle 3. demonstracniho cviceni a
	-- blokoveho schematu mikrokontroleru v zadani

	-- Programovy citac
	pc_logic: process (RESET, CLK)
	begin
		if (RESET = '1') then
			pc_address <= (others => '0');
		elsif rising_edge(CLK) then
			if (pc_dec = '1') then
				pc_address <= pc_address - pc_dec;
			elsif (pc_inc = '1') then
				pc_address <= pc_address + pc_inc;
			end if;
		end if;
		CODE_ADDR <= pc_address;
	end process;
	
	-- Zacatek a konec while
	cnt_logic: process (RESET, CLK)
	begin
		if (RESET = '1') then
			cnt_reg <= (others => '0');
		elsif rising_edge(CLK) then
			if (cnt_reset = '1') then
				cnt_reg <= X"01";
			elsif (cnt_dec = '1') then
				cnt_reg <= cnt_reg - cnt_dec;
			elsif (cnt_inc = '1') then
				cnt_reg <= cnt_reg + cnt_inc;
			end if;
		end if;
	end process;

	-- Ukazatel do pameti dat
	ptr_logic: process (RESET, CLK)
	begin
		if (RESET = '1') then
			ptr_address <= (others => '0');
		elsif rising_edge(CLK) then
			if (ptr_dec = '1') then
				ptr_address <= ptr_address - ptr_dec;
			elsif (ptr_inc = '1') then
				ptr_address <= ptr_address + ptr_inc;
			end if;
		end if;
		DATA_ADDR <= ptr_address;
	end process;

	-- Synchronizacni logika pro konecny automat
	sync_logic: process (RESET, CLK)
	begin
		if (RESET = '1') then
			present_state <= S_FETCH;
		-- Procesor pracuje vzdy pri vzestupne hrane
		-- a vykonavat program zacne tehdy, kdyz je
		-- RESET uvolnen a EN povolen
		elsif rising_edge(CLK) and (EN = '1') then
			present_state <= next_state;
		end if;
	end process;

	-- Logika pro zmenu stavu konecneho automatu
	next_state_logic: process (present_state, CODE_DATA, DATA_RDATA, IN_DATA, IN_VLD, OUT_BUSY)
	begin
		-- Vychozi nastaveni jednotlivych signalu
		pc_dec <= '0';
		pc_inc <= '0';
		ptr_dec <= '0';
		ptr_inc <= '0';
		cnt_reset <= '0';
		cnt_dec <= '0';
		cnt_inc <= '0';
		CODE_EN <= '0';
		DATA_EN <= '0';
		OUT_WE <= '0';

		-- Procesor provadi porad dokola FETCH, DECODE,
		-- EXECUTE (,MEMORY ACCESS, WRITE BACK)
		case (present_state) is
			when S_FETCH =>
				CODE_EN <= '1';
				next_state <= S_DECODE;

			-- Dekodovani hexadecimalniho tvaru instrukce
			when S_DECODE =>
				case (CODE_DATA(7 downto 4)) is
					when X"3" =>
						case (CODE_DATA(3 downto 0)) is
							when X"E" => next_state <= S_PTR_INC;
							when X"C" => next_state <= S_PTR_DEC;
							when others => next_state <= S_COMMENT;
						end case;
					when X"2" =>
						case (CODE_DATA(3 downto 0)) is
							when X"B" => next_state <= S_VAL_INC;
							when X"D" => next_state <= S_VAL_DEC;
							when X"E" => next_state <= S_PUTCHAR;
							when X"C" => next_state <= S_GETCHAR;
							when others => next_state <= S_COMMENT;
						end case;
					when X"5" =>
						case (CODE_DATA(3 downto 0)) is
							when X"B" => next_state <= S_WHILE_BEGIN;
							when X"D" => next_state <= S_WHILE_END;
							when others => next_state <= S_COMMENT;
						end case;
					when X"7" =>
						case (CODE_DATA(3 downto 0)) is
							when X"E" => next_state <= S_BREAK;
							when others => next_state <= S_COMMENT;
						end case;
					when X"0" =>
						case (CODE_DATA(3 downto 0)) is
							when X"0" => next_state <= S_END;
							when others => next_state <= S_COMMENT;
						end case;
					when others => next_state <= S_COMMENT;
				end case;

			-- Jednotlive prikazy jsem se snazil implementovat
			-- podle pseudokodu uvedenem v zadani projektu

			-- Prikaz '>'
			when S_PTR_INC =>
				pc_inc <= '1';
				ptr_inc <= '1';
				next_state <= S_FETCH;

			-- Prikaz '<'
			when S_PTR_DEC =>
				pc_inc <= '1';
				ptr_dec <= '1';
				next_state <= S_FETCH;

			-- Prikaz '+'
			-- Hodnoty nelze upravit primo tady, jelikoz se s nimi pracuje
			-- v jinem procesu, coz by zpusobilo nesyntetizovatelnost kodu
			when S_VAL_INC =>
				DATA_EN <= '1';
				-- DATA_RDWR se nastavuje na '0', pokud cteme
				-- a na '1', pokud zapisujeme
				DATA_RDWR <= '0';
				next_state <= S_VAL_INC_2;
			when S_VAL_INC_2 =>
				-- Pri zapisu na DATA_WDATA dame hodnotu,
				-- kterou chceme zapsat na adresu DATA_ADDR v RAM
				DATA_WDATA <= DATA_RDATA + X"01";
				DATA_EN <= '1';
				DATA_RDWR <= '1';
				pc_inc <= '1';
				next_state <= S_FETCH;

			-- Prikaz '-'
			when S_VAL_DEC =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';
				next_state <= S_VAL_DEC_2;
			when S_VAL_DEC_2 =>
				DATA_EN <= '1';
				DATA_RDWR <= '1';
				DATA_WDATA <= DATA_RDATA - X"01";
				pc_inc <= '1';
				next_state <= S_FETCH;

			-- Prikaz '['
			-- Duvod rozdeleni do tolika vetvi je ten,
			-- ze nelze napriklad cist a zapisovat na RAM
			-- v jednom taktu
			when S_WHILE_BEGIN =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';
				pc_inc <= '1';
				next_state <= S_WHILE_BEGIN_2;
			when S_WHILE_BEGIN_2 =>
				if (DATA_RDATA = X"00") then
					cnt_reset <= '1';
					next_state <= S_WHILE_BEGIN_3;
				else
					next_state <= S_FETCH;
				end if;
			when S_WHILE_BEGIN_3 =>
				if (cnt_reg = X"00") then
					next_state <= S_FETCH;
				else
					CODE_EN <= '1';
					next_state <= S_WHILE_BEGIN_4;
				end if;
			when S_WHILE_BEGIN_4 =>
				if (CODE_DATA = X"5B") then
					cnt_inc <= '1';
				elsif (CODE_DATA = X"5D") then
					cnt_dec <= '1';
				end if;
				pc_inc <= '1';
				-- Cyklim, dokud nedojde k vyrovnani levych
				-- a pravych zavorek
				next_state <= S_WHILE_BEGIN_3;

			-- Prikaz ']'
			-- Rozvetveni je ze stejneho duvodu jako
			-- u prikazu vyse
			when S_WHILE_END =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';
				next_state <= S_WHILE_END_2;
			when S_WHILE_END_2 =>
				if (DATA_RDATA = X"00") then
					pc_inc <= '1';
					next_state <= S_FETCH;
				else
					pc_dec <= '1';
					cnt_reset <= '1';
					next_state <= S_WHILE_END_3;
				end if;
			when S_WHILE_END_3 =>
				if (cnt_reg = X"00") then
					next_state <= S_FETCH;
				else
					CODE_EN <= '1';
					next_state <= S_WHILE_END_4;
				end if;
			when S_WHILE_END_4 =>
				if (CODE_DATA = X"5D") then
					cnt_inc <= '1';
				elsif (CODE_DATA = X"5B") then
					cnt_dec <= '1';
				end if;
				next_state <= S_WHILE_END_5;
			when S_WHILE_END_5 =>
				if (cnt_reg = X"00") then
					pc_inc <= '1';
				else
					pc_dec <= '1';
				end if;
				-- Cyklim, dokud nedojde k vyrovnani levych
				-- a pravych zavorek
				next_state <= S_WHILE_END_3;

			-- Prikaz '.'
			when S_PUTCHAR =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';
				next_state <= S_PUTCHAR_2;
			when S_PUTCHAR_2 =>
				-- Na LCD lze vypisovat pouze tehdy,
				-- pokud neni display zaneprazdnen,
				-- v opacnem pripade cyklim
				if (OUT_BUSY = '1') then
					next_state <= S_PUTCHAR_2;
				else
					OUT_DATA <= DATA_RDATA;
					OUT_WE <= '1';
					pc_inc <= '1';
					next_state <= S_FETCH;
				end if;

			-- Prikaz ','
			when S_GETCHAR =>
				IN_REQ <= '1';
				next_state <= S_GETCHAR_2;
			when S_GETCHAR_2 =>
				if (IN_VLD = '1') then
					DATA_EN <= '1';
					DATA_RDWR <= '1';
					DATA_WDATA <= IN_DATA;
					IN_REQ <= '0';
					pc_inc <= '1';
					next_state <= S_FETCH;
				else
					next_state <= S_GETCHAR_2;
				end if;

			-- Prikaz '~'
			when S_BREAK =>
				pc_inc <= '1';
				cnt_reset <= '1';
				next_state <= S_BREAK_2;
			when S_BREAK_2 =>
				if (cnt_reg = X"00") then
					next_state <= S_FETCH;
				else
					CODE_EN <= '1';
					next_state <= S_BREAK_3;
				end if;
			when S_BREAK_3 =>
				if (CODE_DATA = X"5B") then
					cnt_inc <= '1';
				elsif (CODE_DATA = X"5D") then
					cnt_dec <= '1';
				end if;
				pc_inc <= '1';
				-- Cyklim, dokud nedojde k vyrovnani levych
				-- a pravych zavorek
				next_state <= S_BREAK_2;

			-- Prikaz 'null'
			when S_END =>
				next_state <= S_END;

			-- Jakykoliv jiny znak je bran jako komentar
			when S_COMMENT =>
				pc_inc <= '1';
				next_state <= S_FETCH;
		end case;
	end process;
	
end behavioral;
