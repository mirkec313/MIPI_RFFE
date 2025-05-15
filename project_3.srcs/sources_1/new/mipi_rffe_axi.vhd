library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mipi_rffe_axi is
	generic (
		-- Users to add parameters here

		-- User parameters ends
		-- Do not modify the parameters beyond this line

		-- Width of S_AXI data bus
		C_S_AXI_DATA_WIDTH	: integer	:= 32;
		-- Width of S_AXI address bus
		C_S_AXI_ADDR_WIDTH	: integer	:= 5
	);
	port (
		-- Users to add ports here
		mipi_sclk : out std_logic;
		mipi_sdio : inout std_logic;
		mipi_done : out std_logic;
		mipi_sdio_dbg : out std_logic;
		rdy_dbg : out std_logic;

		-- User ports ends
		-- Do not modify the ports beyond this line

		-- Global Clock Signal
		S_AXI_ACLK	: in std_logic;
		-- Global Reset Signal. This Signal is Active LOW
		S_AXI_ARESETN	: in std_logic;
		-- Write address (issued by master, acceped by Slave)
		S_AXI_AWADDR	: in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
		-- Write channel Protection type. This signal indicates the
    		-- privilege and security level of the transaction, and whether
    		-- the transaction is a data access or an instruction access.
		S_AXI_AWPROT	: in std_logic_vector(2 downto 0);
		-- Write address valid. This signal indicates that the master signaling
    		-- valid write address and control information.
		S_AXI_AWVALID	: in std_logic;
		-- Write address ready. This signal indicates that the slave is ready
    		-- to accept an address and associated control signals.
		S_AXI_AWREADY	: out std_logic;
		-- Write data (issued by master, acceped by Slave)
		S_AXI_WDATA	: in std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
		-- Write strobes. This signal indicates which byte lanes hold
    		-- valid data. There is one write strobe bit for each eight
    		-- bits of the write data bus.
		S_AXI_WSTRB	: in std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
		-- Write valid. This signal indicates that valid write
    		-- data and strobes are available.
		S_AXI_WVALID	: in std_logic;
		-- Write ready. This signal indicates that the slave
    		-- can accept the write data.
		S_AXI_WREADY	: out std_logic;
		-- Write response. This signal indicates the status
    		-- of the write transaction.
		S_AXI_BRESP	: out std_logic_vector(1 downto 0);
		-- Write response valid. This signal indicates that the channel
    		-- is signaling a valid write response.
		S_AXI_BVALID	: out std_logic;
		-- Response ready. This signal indicates that the master
    		-- can accept a write response.
		S_AXI_BREADY	: in std_logic;
		-- Read address (issued by master, acceped by Slave)
		S_AXI_ARADDR	: in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
		-- Protection type. This signal indicates the privilege
    		-- and security level of the transaction, and whether the
    		-- transaction is a data access or an instruction access.
		S_AXI_ARPROT	: in std_logic_vector(2 downto 0);
		-- Read address valid. This signal indicates that the channel
    		-- is signaling valid read address and control information.
		S_AXI_ARVALID	: in std_logic;
		-- Read address ready. This signal indicates that the slave is
    		-- ready to accept an address and associated control signals.
		S_AXI_ARREADY	: out std_logic;
		-- Read data (issued by slave)
		S_AXI_RDATA	: out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
		-- Read response. This signal indicates the status of the
    		-- read transfer.
		S_AXI_RRESP	: out std_logic_vector(1 downto 0);
		-- Read valid. This signal indicates that the channel is
    		-- signaling the required read data.
		S_AXI_RVALID	: out std_logic;
		-- Read ready. This signal indicates that the master can
    		-- accept the read data and response information.
		S_AXI_RREADY	: in std_logic
	);
end mipi_rffe_axi;

architecture arch_imp of mipi_rffe_axi is

	-- AXI4LITE signals
	signal axi_awaddr	: std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
	signal axi_awready	: std_logic;
	signal axi_wready	: std_logic;
	signal axi_bresp	: std_logic_vector(1 downto 0);
	signal axi_bvalid	: std_logic;
	signal axi_araddr	: std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
	signal axi_arready	: std_logic;
	signal axi_rresp	: std_logic_vector(1 downto 0);
	signal axi_rvalid	: std_logic;

	-- Example-specific design signals
	-- local parameter for addressing 32 bit / 64 bit C_S_AXI_DATA_WIDTH
	-- ADDR_LSB is used for addressing 32/64 bit registers/memories
	-- ADDR_LSB = 2 for 32 bits (n downto 2)
	-- ADDR_LSB = 3 for 64 bits (n downto 3)
	constant ADDR_LSB  : integer := (C_S_AXI_DATA_WIDTH/32)+ 1;
	constant OPT_MEM_ADDR_BITS : integer := 2;
	------------------------------------------------
	---- Signals for user logic register space example
	--------------------------------------------------
	signal byte_index	: integer;

    signal mem_logic  : std_logic_vector(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);

	 --State machine local parameters
	constant Idle : std_logic_vector(1 downto 0) := "00";
	constant Raddr: std_logic_vector(1 downto 0) := "10";
	constant Rdata: std_logic_vector(1 downto 0) := "11";
	constant Waddr: std_logic_vector(1 downto 0) := "10";
	constant Wdata: std_logic_vector(1 downto 0) := "11";
	 --State machine variables
	signal state_read : std_logic_vector(1 downto 0);
	signal state_write: std_logic_vector(1 downto 0);

    ATTRIBUTE X_INTERFACE_PARAMETER : STRING;
    ATTRIBUTE X_INTERFACE_INFO : STRING;
    attribute X_INTERFACE_INFO of mipi_done : signal is "xilinx.com:signal:interrupt:1.0 IRQ INTERRUPT";
    attribute X_INTERFACE_PARAMETER of mipi_done : signal is "SENSITIVITY LEVEL_HIGH";

    signal irqen : std_logic;
    signal irq : std_logic;
    signal irqack : std_logic;
    signal start_pulse : std_logic;

    signal rst : std_logic;
    signal wrdiv, rddiv : std_logic_vector(2 downto 0);
    signal usid : std_logic_vector(3 downto 0);
    signal addr : std_logic_vector(7 downto 0);
    signal txdata : std_logic_vector(7 downto 0);
    signal dir : std_logic;
    signal start : std_logic;

    signal rdy, rdy_dly : std_logic;
    signal rxdata : std_logic_vector(7 downto 0);
    signal err : std_logic;
    signal val : std_logic;

    signal sdio_dbg : std_logic;

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


begin
	-- I/O Connections assignments

	S_AXI_AWREADY	<= axi_awready;
	S_AXI_WREADY	<= axi_wready;
	S_AXI_BRESP	<= axi_bresp;
	S_AXI_BVALID	<= axi_bvalid;
	S_AXI_ARREADY	<= axi_arready;
	S_AXI_RRESP	<= axi_rresp;
	S_AXI_RVALID	<= axi_rvalid;
	mem_logic     <= S_AXI_AWADDR(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB) when (S_AXI_AWVALID = '1') else axi_awaddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);

	-- Implement Write state machine
	-- Outstanding write transactions are not supported by the slave i.e., master should assert bready to receive response on or before it starts sending the new transaction
	 process (S_AXI_ACLK)
	   begin
	     if rising_edge(S_AXI_ACLK) then
	        if S_AXI_ARESETN = '0' then
	          --asserting initial values to all 0's during reset
	          axi_awready <= '0';
	          axi_wready <= '0';
	          axi_bvalid <= '0';
	          axi_bresp <= (others => '0');
	          state_write <= Idle;
	        else
	          case (state_write) is
	             when Idle =>		--Initial state inidicating reset is done and ready to receive read/write transactions
	               if (S_AXI_ARESETN = '1') then
	                 axi_awready <= '1';
	                 axi_wready <= '1';
	                 state_write <= Waddr;
	               else state_write <= state_write;
	               end if;
	             when Waddr =>		--At this state, slave is ready to receive address along with corresponding control signals and first data packet. Response valid is also handled at this state
	               if (S_AXI_AWVALID = '1' and axi_awready = '1') then
	                 axi_awaddr <= S_AXI_AWADDR;
	                 if (S_AXI_WVALID = '1') then
	                   axi_awready <= '1';
	                   state_write <= Waddr;
	                   axi_bvalid <= '1';
	                 else
	                   axi_awready <= '0';
	                   state_write <= Wdata;
	                   if (S_AXI_BREADY = '1' and axi_bvalid = '1') then
	                     axi_bvalid <= '0';
	                   end if;
	                 end if;
	               else
	                 state_write <= state_write;
	                 if (S_AXI_BREADY = '1' and axi_bvalid = '1') then
	                   axi_bvalid <= '0';
	                 end if;
	               end if;
	             when Wdata =>		--At this state, slave is ready to receive the data packets until the number of transfers is equal to burst length
	               if (S_AXI_WVALID = '1') then
	                 state_write <= Waddr;
	                 axi_bvalid <= '1';
	                 axi_awready <= '1';
	               else
	                 state_write <= state_write;
	                 if (S_AXI_BREADY ='1' and axi_bvalid = '1') then
	                   axi_bvalid <= '0';
	                 end if;
	               end if;
	             when others =>      --reserved
	               axi_awready <= '0';
	               axi_wready <= '0';
	               axi_bvalid <= '0';
	           end case;
	        end if;
	      end if;
	 end process;
	-- Implement memory mapped register select and write logic generation
	-- The write data is accepted and written to memory mapped registers when
	-- axi_awready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted. Write strobes are used to
	-- select byte enables of slave registers while writing.
	-- These registers are cleared when reset (active low) is applied.
	-- Slave register write enable is asserted when valid address and data are available
	-- and the slave is ready to accept the write address and write data.


	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then
	    if S_AXI_ARESETN = '0' then
	      wrdiv <= (others => '0');
	      rddiv <= (others => '0');
	      usid <= (others => '0');
	      addr <= (others => '0');
	      txdata <= (others => '0');
	      start_pulse <= '0';
	      dir <= '0';
	      irqen <= '1';
	      irqack <= '0';
	    else
	      irqack <= '0';
	      start_pulse <= '0';
	      if (S_AXI_WVALID = '1') then
	          case (mem_logic) is
	          when b"000" => -- 0x00, Control register
	            if S_AXI_WSTRB(0) = '1' then
	              irqen <= S_AXI_WDATA(0);
	              irqack <= S_AXI_WDATA(1);
	              dir <= S_AXI_WDATA(2);
	              start_pulse <= S_AXI_WDATA(3);
	            end if;
	            if S_AXI_WSTRB(1) = '1' then
	              wrdiv <= S_AXI_WDATA(1*8+wrdiv'length-1 downto 1*8);
	            end if;
	            if S_AXI_WSTRB(2) = '1' then
	              rddiv <= S_AXI_WDATA(2*8+rddiv'length-1 downto 2*8);
	            end if;
	            if S_AXI_WSTRB(3) = '1' then
	              usid <= S_AXI_WDATA(3*8+usid'length-1 downto 3*8);
	            end if;
	          when b"001" => -- 0x04, addr register
	            if S_AXI_WSTRB(0) = '1' then
	              addr <= S_AXI_WDATA(0*8+7 downto 0*8);
	            end if;
	          when b"010" => -- 0x08, TX data register
	            if S_AXI_WSTRB(0) = '1' then
	              txdata <= S_AXI_WDATA(0*8+7 downto 0*8);
	            end if;
	          when b"011" => -- 0x0C, RX data register, read-only
	            -- do nothing
	          when b"100" => -- 0x10, status register, read-only
	            -- do nothing
--	          when b"100" =>
--	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
--	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
--	                -- Respective byte enables are asserted as per write strobes
--	                -- slave registor 4
--	                slv_reg4(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
--	              end if;
--	            end loop;
	          when others =>
	            -- do nothing
	        end case;
	      end if;
	    end if;
	  end if;
	end process;

	-- Implement read state machine
	 process (S_AXI_ACLK)
	   begin
	     if rising_edge(S_AXI_ACLK) then
	        if S_AXI_ARESETN = '0' then
	          --asserting initial values to all 0's during reset
	          axi_arready <= '0';
	          axi_rvalid <= '0';
	          axi_rresp <= (others => '0');
	          state_read <= Idle;
	        else
	          case (state_read) is
	            when Idle =>		--Initial state inidicating reset is done and ready to receive read/write transactions
	                if (S_AXI_ARESETN = '1') then
	                  axi_arready <= '1';
	                  state_read <= Raddr;
	                else state_read <= state_read;
	                end if;
	            when Raddr =>		--At this state, slave is ready to receive address along with corresponding control signals
	                if (S_AXI_ARVALID = '1' and axi_arready = '1') then
	                  state_read <= Rdata;
	                  axi_rvalid <= '1';
	                  axi_arready <= '0';
	                  axi_araddr <= S_AXI_ARADDR;
	                else
	                  state_read <= state_read;
	                end if;
	            when Rdata =>		--At this state, slave is ready to send the data packets until the number of transfers is equal to burst length
	                if (axi_rvalid = '1' and S_AXI_RREADY = '1') then
	                  axi_rvalid <= '0';
	                  axi_arready <= '1';
	                  state_read <= Raddr;
	                else
	                  state_read <= state_read;
	                end if;
	            when others =>      --reserved
	                axi_arready <= '0';
	                axi_rvalid <= '0';
	           end case;
	         end if;
	       end if;
	  end process;

  -- Implement memory mapped register select and read logic generation
  -- Slave register read enable is asserted when valid address is available
  -- and the slave is ready to accept the read address.
  process (axi_araddr, irqen, irq, start, dir, wrdiv, rddiv, usid, addr, txdata, rxdata, rdy, err)
    variable loc_addr :std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
  begin
    -- Address decoding for reading registers
    loc_addr := axi_araddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
    S_AXI_RDATA  <= (others => '0');
    case loc_addr is
      when b"000" => -- 0x00, Control register
        S_AXI_RDATA(0) <= irqen;
        S_AXI_RDATA(2) <= dir;
        S_AXI_RDATA(3) <= start;
	    S_AXI_RDATA(1*8+wrdiv'length-1 downto 1*8) <= wrdiv;
	    S_AXI_RDATA(2*8+rddiv'length-1 downto 2*8) <= rddiv;
	    S_AXI_RDATA(3*8+usid'length-1 downto 3*8) <= usid;
	  when b"001" => -- 0x04, Address register
	    S_AXI_RDATA(addr'range) <= addr;
      when b"010" => -- 0x08, TX data register
        S_AXI_RDATA(txdata'range) <= txdata;
      when b"011" => -- 0x0C, RX data register, read-only
        S_AXI_RDATA(rxdata'range) <= rxdata;
      when b"100" => -- 0x10, Status register, read-only
        S_AXI_RDATA(0) <= rdy;
        S_AXI_RDATA(1) <= err;
        S_AXI_RDATA(2) <= irq;
      when others =>
        S_AXI_RDATA  <= (others => '0');
    end case;
  end process;

	-- Add user logic here
	--debug
	mipi_sdio_dbg <= sdio_dbg;
	mipi_done <= irq;
	rdy_dbg <= rdy;
	
	process(S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then
	    if S_AXI_ARESETN = '0' then
	      irq <= '0';
	      rdy_dly <= '1';
	    else
	      rdy_dly <= rdy;
	      if rdy = '1' and rdy_dly = '0' then -- rising edge rdy
	        irq <= '1';
	      elsif irqack = '1' then
	        irq <= '0';
	      end if;
	    end if;
	  end if;
	end process;

    process(S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then
	    if S_AXI_ARESETN = '0' then
	      start <= '0';
	    else
	      if start_pulse = '1' and rdy = '1' then
	        start <= '1';
	      else
	        start <= '0';
	      end if;
	    end if;
	  end if;
	end process;

	rst <= not S_AXI_ARESETN;
	
    mipi_rffe_master: rffe_master
    generic map (
      addr_bits => addr'length,
      data_bits => txdata'length
    )
    port map(
      iclk => S_AXI_ACLK,
      irst => rst,
      iwrdiv => wrdiv,
      irddiv => rddiv,
      iusid => usid,
      iaddr => addr,
      idata => txdata,
      idir => dir,
      istart => start,
      ordy => rdy,
      odata => rxdata,
      oerr => err,
      oval => val,

      sdio => mipi_sdio,
      osclk => mipi_sclk,

      sdio_dbg => sdio_dbg
    );


	-- User logic ends

end arch_imp;