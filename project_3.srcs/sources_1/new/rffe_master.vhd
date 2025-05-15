--FUNCTIONING EXPECT FOR CLKDIV 2


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

use ieee.math_real.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
library UNISIM;
use UNISIM.VComponents.all;

Library xpm;
use xpm.vcomponents.all;

entity rffe_master is
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
end rffe_master;

architecture Behavioral of rffe_master is
  function max(a : natural; b: natural) return natural is
  begin
    if a > b then
      return a;
    end if;
    return b;
  end function;

  constant cmd_read : unsigned(7 downto 0) := "00100000"; -- Extended register read 1 byte
  constant cmd_write : unsigned(7 downto 0) := (others => '0'); -- Extended register write 1 byte

  signal busy : std_logic;
  signal divcnt, divcnt_next : unsigned(2**max(iwrdiv'length, irddiv'length)-1 downto 0);
  signal bitcnt, bitcnt_next : unsigned(natural(ceil(log2(real(max(addr_bits, data_bits)))))-1 downto 0);

  signal rdiv, rdiv_flip : natural range 1 to 2**(2**irddiv'length)-1;
  signal wdiv, wdiv_flip : natural range 1 to 2**(2**iwrdiv'length)-1;

  signal addr_reg : unsigned(iaddr'range);
  signal data_reg : unsigned(idata'range);
  signal usid_reg : unsigned(iusid'range);
  signal cmd_reg  : unsigned(7 downto 0);

  signal sclk : std_logic;
  signal sdo, sdi : std_logic;
  signal sdoe, sdTen : std_logic;

  signal parity : std_logic;
  --signal data_in : std_logic_vector(idata'range);
	
	signal flip_flag   : std_logic;
	signal being_read  : std_logic;
	signal divw        : std_logic;
	signal divr        : std_logic;
	signal divwf       : std_logic;
	signal divrf       : std_logic;
	signal cnt_rst     : std_logic;
	signal cnt_rst_buf : std_logic;
	signal cnt_rst_tmp : std_logic;
    signal switch_state: std_logic;
	
	signal cnt_actual_rst:std_logic;

  type busstate_t is (IDLE, SSC1, SSC2, USID, CMDF, CMDF_PARITY, ADDRF, ADDRF_PARITY, DATA_WRF, DATA_WRF_PARITY, DATA_RDF, DATA_RDF_PARITY, BPC, BPC_RD); --
  signal busstate : busstate_t;

  type cmd_t is (READ, WRITE);
  signal cmd : cmd_t;
begin

  divcnt_next <= divcnt + 1;
  bitcnt_next <= bitcnt + 1;

  ordy <= not busy;
  osclk <= sclk;
  --sdio <= sdo when sdoe = '1' else 'Z';
  sdTen <= not sdoe;

  sdio_dbg <= sdi;

  IOBUF_sdio : IOBUF
  port map (
    O => sdi,       -- 1-bit output: Buffer output
    I => sdo,       -- 1-bit input: Buffer input
    IO => sdio,     -- 1-bit inout: Buffer inout (connect directly to top-level port)
    T => sdTen      -- 1-bit input: 3-state enable input
  );

  process(irddiv)
    variable divval : natural;
  begin
    divval := to_integer(unsigned(irddiv))+1;
    rdiv <= 2**divval-1;
    rdiv_flip <= 2**(divval-1);
  end process;

  process(iwrdiv)
    variable divval : natural;
  begin
    divval := to_integer(unsigned(iwrdiv))+1;
    wdiv <= 2**divval-1;
    wdiv_flip <= 2**(divval-1);
  end process;
 
  --clocking and counter process
  process(iclk)
  begin
        if irst = '1' then
            sclk <= '0';
            divr <= '0';
            divw <= '0';
            divrf <= '0';
            divwf <= '0';
        else
            if rising_edge(iclk) then
                --beginning of clock counting
                if busstate = IDLE then
                    if istart = '1' then
                            divcnt <= (others => '0');
                    else
                    divcnt <= (others => '1');
                    end if;
                else
                    divcnt <= divcnt_next;
                end if;
                
                --when received response from fsm
                if cnt_rst = '1' then
                    divcnt <= (others => '0');
                else
                end if;
                
                --broadcast flags for counter goals
                if divcnt = wdiv-2 then
                    divw <= '1';
                else
                    divw <= '0';
                end if;
                
                if divcnt = rdiv-2 then
                    divr <= '1';
                else
                    divr <= '0';
                end if;
                
                
                if divcnt = wdiv_flip then
                    divwf <= '1';
                else
                    divwf <= '0';
                end if;
                
                
                if divcnt = rdiv_flip then
                    divrf <= '1';
                else
                    divrf <= '0';
                end if;
                
                if flip_flag = '1' then
                    if being_read = '1' then
                        if divcnt = rdiv_flip or divcnt = 0 then
                            sclk <= not sclk;
                        end if;
                    else
                        if divcnt = wdiv_flip or divcnt = 0 then
                            sclk <= not sclk;
                        end if;
                    end if;
                 else
                    sclk <= '0';
                 end if;
             end if;
         end if;        
   end process;
		

			
  process(iclk)
  begin	
    if rising_edge(iclk) then
      if cnt_rst = '1' then
        cnt_rst <= '0';
      else 
        null;
      end if;
      
      
      if irst = '1' then
        
        busy <= '0';
        bitcnt <= (others => '0');
        addr_reg <= (others => '0');
        data_reg <= (others => '0');
        odata <= (others => '0');
        sdo <= '0';
        sdoe <= '0';
        parity <= '0';
        cmd <= READ;
        cnt_rst <= '0';
        oerr <= '0';
		flip_flag <= '0';
		being_read <= '0';
        oval <= '0';
				
      else
        oval <= '0';
        case busstate is
				
				
        when IDLE =>
          if istart = '1' then
            parity <= '0';
            busy <= '1';
            busstate <= SSC1;
            addr_reg <= unsigned(iaddr);
            data_reg <= unsigned(idata);
            usid_reg <= unsigned(iusid);
            sdo <= '0';
            sdoe <= '1';
            oerr <= '0';
            odata <= (others => '0');
            if idir = '0' then
              cmd <= READ;
              cmd_reg <= cmd_read;
            else
              cmd <= WRITE;
              cmd_reg <= cmd_write;
            end if;
          end if;
					
					
					
        when SSC1 =>
          sdo <= '1';
          if divw = '1' then
            cnt_rst <= '1';
          else 
            null;
          end if;
          
          if cnt_rst = '1' then
            busstate <= SSC2;
          else
          end if;
					
					
        when SSC2 =>
          sdo <= '0';
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
          	flip_flag <= '1';
            busstate <= USID;
          else
          end if;
					
					
					
					
        when USID =>
          sdo <= usid_reg(usid_reg'high);
          
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
            if bitcnt = usid_reg'length-1 then
              busstate <= CMDF;
              bitcnt <= (others => '0');
            else
              bitcnt <= bitcnt_next;
              usid_reg <= shift_left(usid_reg, 1);
            end if;
            parity <= parity xor usid_reg(usid_reg'high);
          else
            null;
          end if;
					
					
					
					
					
        when CMDF =>
          sdo <= cmd_reg(cmd_reg'high);
          
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
            if bitcnt = cmd_reg'length-1 then
              busstate <= CMDF_PARITY;
              bitcnt <= (others => '0');
            else
              bitcnt <= bitcnt_next;
              cmd_reg <= shift_left(cmd_reg, 1);
            end if;
            parity <= parity xor cmd_reg(cmd_reg'high);
          else
            null;
          end if;
					
					

					
        when CMDF_PARITY =>
          sdo <= not parity;
          
          
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
          	parity <= '0';
            busstate <= ADDRF;
          else
          end if;
					
					
					
					
        when ADDRF =>
          sdo <= addr_reg(addr_reg'high);
          
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
            if bitcnt = addr_bits-1 then
              busstate <= ADDRF_PARITY;
              bitcnt <= (others => '0');
            else
              bitcnt <= bitcnt_next;
              addr_reg <= shift_left(addr_reg, 1);
            end if;
            parity <= parity xor addr_reg(addr_reg'high);
          else
            null;
          end if;
					
					
					
					
        when ADDRF_PARITY =>
          sdo <= not parity;
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
            parity <= '0';
            if cmd = READ then
              busstate <= BPC_RD;
            else
              busstate <= DATA_WRF;
            end if;
          else
            null;
          end if;
					
					
					
					
					
        when DATA_WRF =>
          sdo <= data_reg(addr_reg'high);
          
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
            if bitcnt = data_bits-1 then
              busstate <= DATA_WRF_PARITY;
              bitcnt <= (others => '0');
            else
              bitcnt <= bitcnt_next;
              data_reg <= shift_left(data_reg, 1);
            end if;
            parity <= parity xor data_reg(data_reg'high);
          else
            null;
          end if;
					
					
					
					
        when DATA_WRF_PARITY =>
          sdo <= not parity;
          
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
            parity <= '0';
            busstate <= BPC;
          else
            null;
          end if;
					
					
					
					
        when DATA_RDF =>
        if divr = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          
          if cnt_rst = '1' then
            if bitcnt = data_bits-1 then
              busstate <= DATA_RDF_PARITY;
              bitcnt <= (others => '0');
            else
              bitcnt <= bitcnt_next;
            end if;
          
          data_reg <= shift_left(data_reg, 1);
          data_reg(0) <= sdi;
          parity <= parity xor sdi;
          
          else
            null;
          end if;

							
					
        when DATA_RDF_PARITY =>
          if divr = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
            busstate <= BPC;
						being_read <= '0';
            odata <= std_logic_vector(data_reg);
            oval <= '1';
          else
            null;
          end if;          
          
          if divrf = '1' then
            if sdi = parity then
              oerr <= '1';
            end if;
          end if;
					
					
					
					
					
        when BPC_RD =>
          sdo <= '0';
          
          if divwf = '1' then
            sdoe <= '0';
          end if;
          
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          if cnt_rst = '1' then
            busstate <= DATA_RDF;
						being_read <= '1';
            data_reg <= (others => '0');
          else
            null;
          end if;
					
									
        when BPC =>
          sdo <= '0';

          if divwf = '1' then
            sdo <= '0';
            sdoe <= '0';
          end if;
          
          if divw = '1' then
            cnt_rst <= '1';
          else
            null;
          end if;
          
          if cnt_rst = '1' then
            busy <= '0';
						flip_flag <= '0';
            busstate <= IDLE;
          else
            null;
          end if;
        end case;
      end if;
    end if;
  end process;

end Behavioral;
