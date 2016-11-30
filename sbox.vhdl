-------------------------------------------------------------------------------
-- Title      : Ascon's 5-bit S-Box
-- Project    : 
-------------------------------------------------------------------------------
-- File       : sbox.vhdl
-- Author     : Hannes Gross
-- Company    : Graz University of Technology
-- Created    : 2016-11-17
-- Last update: 2016-11-23
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: A generic implementation of Ascon's S-box with DOM protection
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-17  1.0      hgross	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.ascon_pkg.all;

-------------------------------------------------------------------------------

entity sbox is

  generic (
    SKIP_AFFINE : string  := "yes";        -- "yes", or "no"
    PIPELINED   : string  := "yes";       -- "yes", or "no" 
    D           : integer := 5            -- Protection order
    );

  port (
    ClkxCI   : in std_logic;
    RstxBI   : in std_logic;
    -- Data inputs (shared)
    X0xDI    : in t_shared_bit(D downto 0);
    X1xDI    : in t_shared_bit(D downto 0);
    X2xDI    : in t_shared_bit(D downto 0);
    X3xDI    : in t_shared_bit(D downto 0);
    X4xDI    : in t_shared_bit(D downto 0);
    -- Fresh random Z masks
    Z0xDI    : in t_shared_bit(NUM_MASKS(D)-1 downto 0);
    Z1xDI    : in t_shared_bit(NUM_MASKS(D)-1 downto 0);
    Z2xDI    : in t_shared_bit(NUM_MASKS(D)-1 downto 0);
    Z3xDI    : in t_shared_bit(NUM_MASKS(D)-1 downto 0);
    Z4xDI    : in t_shared_bit(NUM_MASKS(D)-1 downto 0);
    -- Data outputs (shared)
    X0xDO    : out t_shared_bit(D downto 0);
    X1xDO    : out t_shared_bit(D downto 0);
    X2xDO    : out t_shared_bit(D downto 0);
    X3xDO    : out t_shared_bit(D downto 0);
    X4xDO    : out t_shared_bit(D downto 0)
    );

end entity sbox;

-------------------------------------------------------------------------------

architecture beh of sbox is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal L0, L1, L2, L3, L4                : t_shared_bit(D downto 0);  -- 1. input to DOM AND
  signal nL0, nL1, nL2, nL3, nL4           : t_shared_bit(D downto 0);  -- 2. input
  signal A0, A1, A2, A3, A4                : t_shared_bit(D downto 0);  -- output of DOM AND
  -- pipeline stage registers
  signal L0stage0, L1stage0, L2stage0, L3stage0, L4stage0 : t_shared_bit(D downto 0);
  signal L0stage1, L1stage1, L2stage1, L3stage1, L4stage1 : t_shared_bit(D downto 0);
  -- select input of non-linear part of S-box
  signal I0, I1, I2, I3, I4 : t_shared_bit(D downto 0);
begin  -- architecture str

  -- purpose: Perform s-box lookup
  sbox_p : process (A0, A1, A2, A3, A4, L0, L1, L2,
                    L3, L4, X0xDI, X1xDI, X2xDI, X3xDI, X4xDI,
                    L0stage0, L1stage0, L2stage0, L3stage0, L4stage0,
                    L0stage1, L1stage1, L2stage1, L3stage1, L4stage1,
                    I0, I1, I2, I3, I4) is
    -- Calculate Sbox in four steps
    variable B0, B1, B2, B3, B4 : t_shared_bit(D downto 0); 
  begin  -- process sbox_p
    -- for each share
    for i in D downto 0 loop
      -- 0. Affine transformation at begin
      L0(i) <= X0xDI(i) xor X4xDI(i);
      L1(i) <= X1xDI(i);
      L2(i) <= X2xDI(i) xor X1xDI(i);
      L3(i) <= X3xDI(i);
      L4(i) <= X4xDI(i) xor X3xDI(i);

      -- Skip affine transformation?
      if(SKIP_AFFINE = "yes") then
        I0(i) <= X0xDI(i);
        I1(i) <= X1xDI(i);
        I2(i) <= X2xDI(i);
        I3(i) <= X3xDI(i);
        I4(i) <= X4xDI(i);
      else
        I0(i) <= L0stage0(i);
        I1(i) <= L1stage0(i);
        I2(i) <= L2stage0(i);
        I3(i) <= L3stage0(i);
        I4(i) <= L4stage0(i);
      end if;

      -- inverse signals (only first share needs to be inverted)
      if (i = 0) then
        nL0(i) <= not I0(i);
        nL1(i) <= not I1(i);
        nL2(i) <= not I2(i);
        nL3(i) <= not I3(i);
        nL4(i) <= not I4(i);
      else
        nL0(i) <=  I0(i);
        nL1(i) <=  I1(i);
        nL2(i) <=  I2(i);
        nL3(i) <=  I3(i);
        nL4(i) <=  I4(i);
      end if;

      -- 2. Linear part with or without pipelining
      if (PIPELINED = "yes") then
        B0(i) := L0stage1(i) xor A1(i);
        B1(i) := L1stage1(i) xor A2(i);
        B2(i) := L2stage1(i) xor A3(i);
        B3(i) := L3stage1(i) xor A4(i);
        B4(i) := L4stage1(i) xor A0(i);
      else
        B0(i) := L0(i) xor A1(i);
        B1(i) := L1(i) xor A2(i);
        B2(i) := L2(i) xor A3(i);
        B3(i) := L3(i) xor A4(i);
        B4(i) := L4(i) xor A0(i);
      end if;
      
      -- 3. Linear output transformation
      X0xDO(i) <= B0(i) xor B4(i);
      X1xDO(i) <= B1(i) xor B0(i);
      if (i = 0) then -- only invert first share
        X2xDO(i) <= not B2(i);
      else
        X2xDO(i) <= B2(i);
      end if;
      X3xDO(i) <= B3(i) xor B2(i);
      X4xDO(i) <= B4(i);    
    end loop;
  end process sbox_p;
  
  -----------------------------------------------------------------------------
  -- Optional pipelining registers
  -----------------------------------------------------------------------------
  pipelining_reg_p: process (ClkxCI, RstxBI) is
  begin  -- process pipelining_reg_p
    -- for each share
    for i in D downto 0 loop
      if RstxBI = '0' then              -- asynchronous reset (active low)
        L0stage0 <= (others => '0');    --Stage 0
        L1stage0 <= (others => '0');
        L2stage0 <= (others => '0');
        L3stage0 <= (others => '0');
        L4stage0 <= (others => '0');
        L0stage1 <= (others => '0');    --Stage 1
        L1stage1 <= (others => '0');
        L2stage1 <= (others => '0');
        L3stage1 <= (others => '0');
        L4stage1 <= (others => '0');
      elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
        L0stage0(i) <= L0(i);           --Stage 0
        L1stage0(i) <= L1(i);
        L2stage0(i) <= L2(i);
        L3stage0(i) <= L3(i);
        L4stage0(i) <= L4(i);

        L0stage1(i) <= I0(i);           --Stage 1
        L1stage1(i) <= I1(i);
        L2stage1(i) <= I2(i);
        L3stage1(i) <= I3(i);
        L4stage1(i) <= I4(i);
      end if;
    end loop;  --share_i
  end process pipelining_reg_p;
  
  -----------------------------------------------------------------------------
  -- DOM AND gates
  -----------------------------------------------------------------------------
  not_L0_and_L1: entity work.dom_and
    generic map (
      PIPELINED => PIPELINED,
      D         => D)
    port map (
      ClkxCI => ClkxCI,
      RstxBI => RstxBI,
      XxDI   => nL0,
      YxDI   => I1,
      ZxDI   => Z0xDI,
      QxDO   => A0);

  not_L1_and_L2: entity work.dom_and
    generic map (
      PIPELINED => PIPELINED,
      D         => D)
    port map (
      ClkxCI => ClkxCI,
      RstxBI => RstxBI,
      XxDI   => nL1,
      YxDI   => I2,
      ZxDI   => Z1xDI,
      QxDO   => A1);

  not_L2_and_L3: entity work.dom_and
    generic map (
      PIPELINED => PIPELINED,
      D         => D)
    port map (
      ClkxCI => ClkxCI,
      RstxBI => RstxBI,
      XxDI   => nL2,
      YxDI   => I3,
      ZxDI   => Z2xDI,
      QxDO   => A2);

  not_L3_and_L4: entity work.dom_and
    generic map (
      PIPELINED => PIPELINED,
      D         => D)
    port map (
      ClkxCI => ClkxCI,
      RstxBI => RstxBI,
      XxDI   => nL3,
      YxDI   => I4,
      ZxDI   => Z3xDI,
      QxDO   => A3);

  not_L4_and_L0: entity work.dom_and
    generic map (
      PIPELINED => PIPELINED,
      D         => D)
    port map (
      ClkxCI => ClkxCI,
      RstxBI => RstxBI,
      XxDI   => nL4,
      YxDI   => I0,
      ZxDI   => Z4xDI,
      QxDO   => A4);

end architecture beh;

-------------------------------------------------------------------------------
