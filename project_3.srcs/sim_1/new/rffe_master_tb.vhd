-- Testbench for MIPI RFFE Master
-- Alexander Vickberg
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity rffe_master_tb is
--  Port ( );
end rffe_master_tb;

architecture Behavioral of rffe_master_tb is
component rffe_master is
  generic(
    addr_bits : natural := 4;
    data_bits : natural := 8
  );
  port(
    iclk   : in std_logic;
    irst   : in std_logic;
    iwrdiv : in std_logic_vector(2 downto 0);
    irddiv : in std_logic_vector(2 downto 0);
    iusid  : in std_logic_vector(3 downto 0);
    iaddr  : in std_logic_vector(addr_bits-1 downto 0);
    idata  : in std_logic_vector(data_bits-1 downto 0);
    idir   : in std_logic; -- Read = 0, write = 1
    istart : in std_logic;
    ordy   : out std_logic;
    odata  : out std_logic_vector(data_bits-1 downto 0);
    oerr   : out std_logic;
    oval   : out std_logic;

    sdio   : inout std_logic;
    osclk  : out std_logic;

    -- debug ports
    sdio_dbg : out std_logic
  );
end component;
  constant clk_p : time := 10 ns;
  constant wrclkdiv : natural := 1;
  constant rdclkdiv : natural := 2;
  constant usid : natural := 8;

  signal clk : std_logic := '1';
  signal reset : std_logic := '1';

  constant addr : std_logic_vector(7 downto 0) := x"55";
  constant data : std_logic_vector(7 downto 0) := "01010101";  

  signal dir  : std_logic := '1';
  signal start : std_logic := '0';
  signal rdy : std_logic;

  signal parity : std_logic := '0';
  constant rd_data : unsigned(7 downto 0) := "01011101";

  signal sdio : std_logic := 'Z';
  signal sclk : std_logic;

begin
  clk     <= not clk after clk_p/2;

  process
  begin
    wait for 4*clk_p;
    reset <= '0';
    wait until rising_edge(clk);

    start <= '1';
    wait until rising_edge(clk);
    start <= '0';

    wait until rdy = '1';
    wait until rising_edge(clk);
    dir <= '0';
    start <= '1';
    wait until rising_edge(clk);
    start <= '0';

    for i in 0 to 13+9 loop
      wait until rising_edge(sclk);
    end loop;

    for i in 0 to 7 loop
      wait until rising_edge(sclk);
      sdio <= rd_data(7-i);
    end loop;

    wait until rising_edge(sclk);
    sdio <= parity;

    wait until rising_edge(sclk);
    sdio <= '0';
    
    wait until falling_edge(sclk);
    wait until rising_edge(clk);
    sdio <= 'Z';

    wait;
  end process;

  dut: rffe_master
  generic map (
    addr_bits => addr'length,
    data_bits => data'length
  )
  port map(
    iclk => clk,
    irst => reset,
    iwrdiv => std_logic_vector(to_unsigned(wrclkdiv, 3)),
    irddiv => std_logic_vector(to_unsigned(rdclkdiv, 3)),
    iusid => std_logic_vector(to_unsigned(usid, 4)),
    iaddr => addr,
    idata => data,
    idir => dir,
    istart => start,
    ordy => rdy,
    
    sdio => sdio,
    osclk => sclk
  );
end Behavioral;