-------------------------------------------------------------------------------
-- Title      : Ascon's 5-bit S-Box
-- Project    : 
-------------------------------------------------------------------------------
-- File       : sbox.vhdl
-- Author     : Hannes Gross
-- Company    : Graz University of Technology
-- Created    : 2016-11-17
-- Last update: 2017-01-24
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
    SKIP_AFFINE : string := "no";       -- "yes", or "no"
    PIPELINED   : string := "yes"       -- "yes", or "no" 
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
    Z0xDI    : in t_shared_bit(NUM_MASKS-1 downto 0);
    Z1xDI    : in t_shared_bit(NUM_MASKS-1 downto 0);
    Z2xDI    : in t_shared_bit(NUM_MASKS-1 downto 0);
    Z3xDI    : in t_shared_bit(NUM_MASKS-1 downto 0);
    Z4xDI    : in t_shared_bit(NUM_MASKS-1 downto 0);
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
  -- output of "stage 0" -> affine layer
  signal L0, L1, L2, L3, L4                : t_shared_bit(D downto 0);  
  signal A0, A1, A2, A3, A4                : t_shared_bit(D downto 0);  -- output of DOM AND
  -- Register for linear/affine transformation at begin of S-box
  signal L0xDP, L1xDP, L2xDP, L3xDP, L4xDP : t_shared_bit(D downto 0);
  -- Pipeline register for inputs for "stage 1" of S-box (after affine transformation)
  signal I0xD0, I1xD0, I2xD0, I3xD0, I4xD0 : t_shared_bit(D downto 0);
  signal I0xD1, I1xD1, I2xD1, I3xD1, I4xD1 : t_shared_bit(D downto 0);
  signal I0xD2, I1xD2, I2xD2, I3xD2, I4xD2 : t_shared_bit(D downto 0);
  signal I0xD3, I1xD3, I2xD3, I3xD3, I4xD3 : t_shared_bit(D downto 0);
  signal I0xD4, I1xD4, I2xD4, I3xD4, I4xD4 : t_shared_bit(D downto 0);
  -- Inverted signals (synthesizer is not intelligent enough to remove
  -- unnecessary pipelining regiters)
  signal nI0xD0, nI1xD0, nI2xD0, nI3xD0, nI4xD0 : t_shared_bit(D downto 0);
  signal nI0xD1, nI1xD1, nI2xD1, nI3xD1, nI4xD1 : t_shared_bit(D downto 0);
  signal nI0xD2, nI1xD2, nI2xD2, nI3xD2, nI4xD2 : t_shared_bit(D downto 0);
  signal nI0xD3, nI1xD3, nI2xD3, nI3xD3, nI4xD3 : t_shared_bit(D downto 0);
  signal nI0xD4, nI1xD4, nI2xD4, nI3xD4, nI4xD4 : t_shared_bit(D downto 0);

  -- Pipelined inputs for LR AND gate
  -- regular inputs
  signal I0_0xD, I1_0xD, I2_0xD, I3_0xD, I4_0xD : t_shared_bit(D downto 0);
  signal I0_1xD, I1_1xD, I2_1xD, I3_1xD, I4_1xD : t_shared_bit(D downto 0);
  signal I0_2xD, I1_2xD, I2_2xD, I3_2xD, I4_2xD : t_shared_bit(D downto 0);
  signal I0_3xD, I1_3xD, I2_3xD, I3_3xD, I4_3xD : t_shared_bit(D downto 0);
  signal I0_4xD, I1_4xD, I2_4xD, I3_4xD, I4_4xD : t_shared_bit(D downto 0);
  signal I0_5xD, I1_5xD, I2_5xD, I3_5xD, I4_5xD : t_shared_bit(D downto 0);
  -- inverted inputs
  signal N0_0xD, N1_0xD, N2_0xD, N3_0xD, N4_0xD : t_shared_bit(D downto 0);
  signal N0_1xD, N1_1xD, N2_1xD, N3_1xD, N4_1xD : t_shared_bit(D downto 0);
  signal N0_2xD, N1_2xD, N2_2xD, N3_2xD, N4_2xD : t_shared_bit(D downto 0);
  signal N0_3xD, N1_3xD, N2_3xD, N3_3xD, N4_3xD : t_shared_bit(D downto 0);
  signal N0_4xD, N1_4xD, N2_4xD, N3_4xD, N4_4xD : t_shared_bit(D downto 0);
  signal N0_5xD, N1_5xD, N2_5xD, N3_5xD, N4_5xD : t_shared_bit(D downto 0); 
  
    
 
  -- select input of non-linear part of S-box
  signal I0, I1, I2, I3, I4                     : t_shared_bit(D downto 0);
  signal nI0, nI1, nI2, nI3, nI4                : t_shared_bit(D downto 0);
  -- onyl implement pipelining register for first share for inverted Inputs
  signal nI0_share0xD0, nI1_share0xD0, nI2_share0xD0, nI3_share0xD0, nI4_share0xD0 : std_logic;
  signal nI0_share0xD1, nI1_share0xD1, nI2_share0xD1, nI3_share0xD1, nI4_share0xD1 : std_logic;
  signal nI0_share0xD2, nI1_share0xD2, nI2_share0xD2, nI3_share0xD2, nI4_share0xD2 : std_logic;
  signal nI0_share0xD3, nI1_share0xD3, nI2_share0xD3, nI3_share0xD3, nI4_share0xD3 : std_logic;
  signal nI0_share0xD4, nI1_share0xD4, nI2_share0xD4, nI3_share0xD4, nI4_share0xD4 : std_logic;
begin  -- architecture str

  -- purpose: Perform s-box lookup
  sbox_p : process (A0, A1, A2, A3, A4, L0, L1, L2,
                    L3, L4, X0xDI, X1xDI, X2xDI, X3xDI, X4xDI,
                    L0xDP, L1xDP, L2xDP, L3xDP, L4xDP,
                    I0xD0, I1xD0, I2xD0, I3xD0, I4xD0,
                    I0xD1, I1xD1, I2xD1, I3xD1, I4xD1,
                    I0xD2, I1xD2, I2xD2, I3xD2, I4xD2,
                    I0xD3, I1xD3, I2xD3, I3xD3, I4xD3,
                    I0xD4, I1xD4, I2xD4, I3xD4, I4xD4,
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
        I0(i) <= L0xDP(i);
        I1(i) <= L1xDP(i);
        I2(i) <= L2xDP(i);
        I3(i) <= L3xDP(i);
        I4(i) <= L4xDP(i);
      end if;

      -- inverse signals (only first share needs to be inverted)
      if (i = 0) then
        nI0(i) <= not I0(i);
        nI1(i) <= not I1(i);
        nI2(i) <= not I2(i);
        nI3(i) <= not I3(i);
        nI4(i) <= not I4(i);
      else
        nI0(i) <=  I0(i);
        nI1(i) <=  I1(i);
        nI2(i) <=  I2(i);
        nI3(i) <=  I3(i);
        nI4(i) <=  I4(i);
      end if;

      -- 2. Linear part with or without pipelining
      if (PIPELINED = "yes") then
        if SBOX_VARIANT = "DOM" then
          B0(i) := I0xD0(i) xor A1(i);
          B1(i) := I1xD0(i) xor A2(i);
          B2(i) := I2xD0(i) xor A3(i);
          B3(i) := I3xD0(i) xor A4(i);
          B4(i) := I4xD0(i) xor A0(i);
        else
          B0(i) := I0xD4(i) xor A1(i);
          B1(i) := I1xD4(i) xor A2(i);
          B2(i) := I2xD4(i) xor A3(i);
          B3(i) := I3xD4(i) xor A4(i);
          B4(i) := I4xD4(i) xor A0(i);
        end if;
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
        L0xDP <= (others => '0'); 
        L1xDP <= (others => '0');
        L2xDP <= (others => '0');
        L3xDP <= (others => '0');
        L4xDP <= (others => '0');
        -- Pipelining stage of inputs of "stage 2" of S-box
        I0xD0 <= (others => '0'); -- Stage 0
        I1xD0 <= (others => '0');
        I2xD0 <= (others => '0');
        I3xD0 <= (others => '0');
        I4xD0 <= (others => '0');
        I0xD1 <= (others => '0'); -- Stage 1
        I1xD1 <= (others => '0');
        I2xD1 <= (others => '0');
        I3xD1 <= (others => '0');
        I4xD1 <= (others => '0');
        I0xD2 <= (others => '0'); -- Stage 2
        I1xD2 <= (others => '0');
        I2xD2 <= (others => '0');
        I3xD2 <= (others => '0');
        I4xD2 <= (others => '0');
        I0xD3 <= (others => '0'); -- Stage 3
        I1xD3 <= (others => '0');
        I2xD3 <= (others => '0');
        I3xD3 <= (others => '0');
        I4xD3 <= (others => '0');
        I0xD4 <= (others => '0'); -- Stage 4
        I1xD4 <= (others => '0');
        I2xD4 <= (others => '0');
        I3xD4 <= (others => '0');
        I4xD4 <= (others => '0');
        
        -- Inverse stage (only share 0)
        nI0_share0xD0 <= '0'; -- Stage 0
        nI1_share0xD0 <= '0';
        nI2_share0xD0 <= '0';
        nI3_share0xD0 <= '0';
        nI4_share0xD0 <= '0';
        nI0_share0xD1 <= '0'; -- Stage 1
        nI1_share0xD1 <= '0';
        nI2_share0xD1 <= '0';
        nI3_share0xD1 <= '0';
        nI4_share0xD1 <= '0';
        nI0_share0xD2 <= '0'; -- Stage 2
        nI1_share0xD2 <= '0';
        nI2_share0xD2 <= '0';
        nI3_share0xD2 <= '0';
        nI4_share0xD2 <= '0';
        nI0_share0xD3 <= '0'; -- Stage 3
        nI1_share0xD3 <= '0';
        nI2_share0xD3 <= '0';
        nI3_share0xD3 <= '0';
        nI4_share0xD3 <= '0';
        nI0_share0xD4 <= '0'; -- Stage 4
        nI1_share0xD4 <= '0';
        nI2_share0xD4 <= '0';
        nI3_share0xD4 <= '0';
        nI4_share0xD4 <= '0';    
      elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
        L0xDP(i) <= L0(i);  
        L1xDP(i) <= L1(i);
        L2xDP(i) <= L2(i);
        L3xDP(i) <= L3(i);
        L4xDP(i) <= L4(i);

        
        I0xD0(i) <= I0(i);    -- Stage 0
        I1xD0(i) <= I1(i);
        I2xD0(i) <= I2(i);
        I3xD0(i) <= I3(i);
        I4xD0(i) <= I4(i);
        -- Stage 1
        -- This is the entry point of shift register if delay = 4
        if NUM_PIPELINE_REGS (1) = 4 then
          I0xD1(i) <= I0(i);
          I1xD1(i) <= I1(i);
          I2xD1(i) <= I2(i);
          I3xD1(i) <= I3(i);
          I4xD1(i) <= I4(i);
        else
          I0xD1(i) <= I0xD0(i);
          I1xD1(i) <= I1xD0(i);
          I2xD1(i) <= I2xD0(i);
          I3xD1(i) <= I3xD0(i);
          I4xD1(i) <= I4xD0(i);
        end if;
        -- Stage 2
        -- This is the entry point of shift register if delay = 3
        if NUM_PIPELINE_REGS (1) = 3 then
          I0xD2(i) <= I0(i);
          I1xD2(i) <= I1(i);
          I2xD2(i) <= I2(i);
          I3xD2(i) <= I3(i);
          I4xD2(i) <= I4(i);
        else
          I0xD2(i) <= I0xD1(i);
          I1xD2(i) <= I1xD1(i);
          I2xD2(i) <= I2xD1(i);
          I3xD2(i) <= I3xD1(i);
          I4xD2(i) <= I4xD1(i);
        end if;
        -- Stage 3
        -- This is the entry point of shift register if delay = 2
        if NUM_PIPELINE_REGS (1) = 2 then
          I0xD3(i) <= I0(i);
          I1xD3(i) <= I1(i);
          I2xD3(i) <= I2(i);
          I3xD3(i) <= I3(i);
          I4xD3(i) <= I4(i);
        else
          I0xD3(i) <= I0xD2(i);
          I1xD3(i) <= I1xD2(i);
          I2xD3(i) <= I2xD2(i);
          I3xD3(i) <= I3xD2(i);
          I4xD3(i) <= I4xD2(i);
        end if;
        -- Stage 4
        -- This is the entry point of shift register if delay = 1
        if NUM_PIPELINE_REGS (1) = 1 then
          I0xD4(i) <= I0(i);
          I1xD4(i) <= I1(i);
          I2xD4(i) <= I2(i);
          I3xD4(i) <= I3(i);
          I4xD4(i) <= I4(i);
        else
          I0xD4(i) <= I0xD3(i);
          I1xD4(i) <= I1xD3(i);
          I2xD4(i) <= I2xD3(i);
          I3xD4(i) <= I3xD3(i);
          I4xD4(i) <= I4xD3(i);
        end if;

        -- Inverse stage (pipeline only share 0)
        nI0_share0xD0 <= nI0(0); -- Stage 0
        nI1_share0xD0 <= nI1(0);
        nI2_share0xD0 <= nI2(0);
        nI3_share0xD0 <= nI3(0);
        nI4_share0xD0 <= nI4(0);
        -- Stage 1
        -- This is the entry point of shift register if delay = 4
        if NUM_PIPELINE_REGS (1) = 4 then
          nI0_share0xD1 <= nI0(0);
          nI1_share0xD1 <= nI1(0);
          nI2_share0xD1 <= nI2(0);
          nI3_share0xD1 <= nI3(0);
          nI4_share0xD1 <= nI4(0);
        else
          nI0_share0xD1 <= nI0_share0xD0;
          nI1_share0xD1 <= nI1_share0xD0;
          nI2_share0xD1 <= nI2_share0xD0;
          nI3_share0xD1 <= nI3_share0xD0;
          nI4_share0xD1 <= nI4_share0xD0;
        end if;
        -- Stage 2
        -- This is the entry point of shift register if delay = 3
        if NUM_PIPELINE_REGS (1) = 3 then
          nI0_share0xD2 <= nI0(0);
          nI1_share0xD2 <= nI1(0);
          nI2_share0xD2 <= nI2(0);
          nI3_share0xD2 <= nI3(0);
          nI4_share0xD2 <= nI4(0);
        else
          nI0_share0xD2 <= nI0_share0xD1;
          nI1_share0xD2 <= nI1_share0xD1;
          nI2_share0xD2 <= nI2_share0xD1;
          nI3_share0xD2 <= nI3_share0xD1;
          nI4_share0xD2 <= nI4_share0xD1;
        end if;
        -- Stage 3
        -- This is the entry point of shift register if delay = 2
        if NUM_PIPELINE_REGS (1) = 2 then
          nI0_share0xD3 <= nI0(0);
          nI1_share0xD3 <= nI1(0);
          nI2_share0xD3 <= nI2(0);
          nI3_share0xD3 <= nI3(0);
          nI4_share0xD3 <= nI4(0);
        else
          nI0_share0xD3 <= nI0_share0xD2;
          nI1_share0xD3 <= nI1_share0xD2;
          nI2_share0xD3 <= nI2_share0xD2;
          nI3_share0xD3 <= nI3_share0xD2;
          nI4_share0xD3 <= nI4_share0xD2;
        end if;
        -- Stage 4
        -- This is the entry point of shift register if delay = 1
        if NUM_PIPELINE_REGS (1) = 1 then
          nI0_share0xD4 <= nI0(0);
          nI1_share0xD4 <= nI1(0);
          nI2_share0xD4 <= nI2(0);
          nI3_share0xD4 <= nI3(0);
          nI4_share0xD4 <= nI4(0);
        else
          nI0_share0xD4 <= nI0_share0xD3;  -- Stage 4
          nI1_share0xD4 <= nI1_share0xD3;
          nI2_share0xD4 <= nI2_share0xD3;
          nI3_share0xD4 <= nI3_share0xD3;
          nI4_share0xD4 <= nI4_share0xD3;
        end if;
      end if;
    end loop;  --share_i
  end process pipelining_reg_p;

  -- this process is neccessary because the synthesizer it too dump to remove
  -- unnecessary pipelining registers :(
  -- --> only first shares is pipelined (inverted)
  -- the rest reuses the original pipeline regs
  inverse_I_remaining_shares_p : process (I0xD0, I0xD1, I0xD2,
                                          I0xD3, I0xD4, I1xD0,
                                          I1xD1, I1xD2, I1xD3,
                                          I1xD4, I2xD0, I2xD1,
                                          I2xD2, I2xD3, I2xD4,
                                          I3xD0, I3xD1, I3xD2,
                                          I3xD3, I3xD4, I4xD0,
                                          I4xD1, I4xD2, I4xD3,
                                          I4xD4) is
  begin  -- process inverse_I_remaining_shares_p
    -- purpose:
    for i in D downto 1 loop
      nI0xD0(i) <= I0xD0(i);            -- Stage 0
      nI1xD0(i) <= I1xD0(i);
      nI2xD0(i) <= I2xD0(i);
      nI3xD0(i) <= I3xD0(i);
      nI4xD0(i) <= I4xD0(i);
      nI0xD1(i) <= I0xD1(i);            -- Stage 1
      nI1xD1(i) <= I1xD1(i);
      nI2xD1(i) <= I2xD1(i);
      nI3xD1(i) <= I3xD1(i);
      nI4xD1(i) <= I4xD1(i);
      nI0xD2(i) <= I0xD2(i);            -- Stage 2
      nI1xD2(i) <= I1xD2(i);
      nI2xD2(i) <= I2xD2(i);
      nI3xD2(i) <= I3xD2(i);
      nI4xD2(i) <= I4xD2(i);
      nI0xD3(i) <= I0xD3(i);            -- Stage 3
      nI1xD3(i) <= I1xD3(i);
      nI2xD3(i) <= I2xD3(i);
      nI3xD3(i) <= I3xD3(i);
      nI4xD3(i) <= I4xD3(i);
      nI0xD4(i) <= I0xD4(i);            -- Stage 4
      nI1xD4(i) <= I1xD4(i);
      nI2xD4(i) <= I2xD4(i);
      nI3xD4(i) <= I3xD4(i);
      nI4xD4(i) <= I4xD4(i);
    end loop;

    -- share 0
    nI0xD0(0) <= nI0_share0xD0;              -- Stage 0
    nI1xD0(0) <= nI1_share0xD0;
    nI2xD0(0) <= nI2_share0xD0;
    nI3xD0(0) <= nI3_share0xD0;
    nI4xD0(0) <= nI4_share0xD0;
    nI0xD1(0) <= nI0_share0xD1;              -- Stage 1
    nI1xD1(0) <= nI1_share0xD1;
    nI2xD1(0) <= nI2_share0xD1;
    nI3xD1(0) <= nI3_share0xD1;
    nI4xD1(0) <= nI4_share0xD1;
    nI0xD2(0) <= nI0_share0xD2;              -- Stage 2
    nI1xD2(0) <= nI1_share0xD2;
    nI2xD2(0) <= nI2_share0xD2;
    nI3xD2(0) <= nI3_share0xD2;
    nI4xD2(0) <= nI4_share0xD2;
    nI0xD3(0) <= nI0_share0xD3;              -- Stage 3
    nI1xD3(0) <= nI1_share0xD3;
    nI2xD3(0) <= nI2_share0xD3;
    nI3xD3(0) <= nI3_share0xD3;
    nI4xD3(0) <= nI4_share0xD3;
    nI0xD4(0) <= nI0_share0xD4;              -- Stage 4
    nI1xD4(0) <= nI1_share0xD4;
    nI2xD4(0) <= nI2_share0xD4;
    nI3xD4(0) <= nI3_share0xD4;
    nI4xD4(0) <= nI4_share0xD4;
  end process inverse_I_remaining_shares_p;

  -- Select inputs for lor randomness AND gates
  inputs_for_low_randomness_ands_p : process (I0, I0xD0, I0xD1,
                                              I0xD2, I0xD3, I0xD4,
                                              I1, I1xD0, I1xD1,
                                              I1xD2, I1xD3, I1xD4,
                                              I2, I2xD0, I2xD1,
                                              I2xD2, I2xD3, I2xD4,
                                              I3, I3xD0, I3xD1,
                                              I3xD2, I3xD3, I3xD4,
                                              I4, I4xD0, I4xD1,
                                              I4xD2, I4xD3, I4xD4,
                                              nI0, nI0xD0, nI0xD1,
                                              nI0xD2, nI0xD3, nI0xD4,
                                              nI1, nI1xD0, nI1xD1,
                                              nI1xD2, nI1xD3, nI1xD4,
                                              nI2, nI2xD0, nI2xD1,
                                              nI2xD2, nI2xD3, nI2xD4,
                                              nI3, nI3xD0, nI3xD1,
                                              nI3xD2, nI3xD3, nI3xD4,
                                              nI4, nI4xD0, nI4xD1,
                                              nI4xD2, nI4xD3, nI4xD4) is
  begin  -- process inputs_for_low_randomness_ands_p
    for i in D downto 0 loop
      -- Default (uses 5 pipeline registers for LR AND)
      I0_0xD(i) <= I0(i);               -- first stage inputs
      I1_0xD(i) <= I1(i);
      I2_0xD(i) <= I2(i);
      I3_0xD(i) <= I3(i);
      I4_0xD(i) <= I4(i);
      I0_1xD(i) <= I0xD0(i);            -- second stage inputs
      I1_1xD(i) <= I1xD0(i);
      I2_1xD(i) <= I2xD0(i);
      I3_1xD(i) <= I3xD0(i);
      I4_1xD(i) <= I4xD0(i);
      I0_2xD(i) <= I0xD1(i);            -- third stage inputs
      I1_2xD(i) <= I1xD1(i);
      I2_2xD(i) <= I2xD1(i);
      I3_2xD(i) <= I3xD1(i);
      I4_2xD(i) <= I4xD1(i);
      I0_3xD(i) <= I0xD2(i);            -- fourth stage inputs
      I1_3xD(i) <= I1xD2(i);
      I2_3xD(i) <= I2xD2(i);                         
      I3_3xD(i) <= I3xD2(i);
      I4_3xD(i) <= I4xD2(i);
      I0_4xD(i) <= I0xD3(i);            -- fifth stage inputs
      I1_4xD(i) <= I1xD3(i);
      I2_4xD(i) <= I2xD3(i);
      I3_4xD(i) <= I3xD3(i);
      I4_4xD(i) <= I4xD3(i);
      I0_5xD(i) <= I0xD4(i);            -- sixth stage inputs
      I1_5xD(i) <= I1xD4(i);
      I2_5xD(i) <= I2xD4(i);
      I3_5xD(i) <= I3xD4(i);
      I4_5xD(i) <= I4xD4(i);
      -- inverted inputs
      N0_0xD(i) <= nI0(i);              -- first stage inputs
      N1_0xD(i) <= nI1(i);
      N2_0xD(i) <= nI2(i);
      N3_0xD(i) <= nI3(i);
      N4_0xD(i) <= nI4(i);
      N0_1xD(i) <= nI0xD0(i);           -- second stage inputs
      N1_1xD(i) <= nI1xD0(i);
      N2_1xD(i) <= nI2xD0(i);
      N3_1xD(i) <= nI3xD0(i);
      N4_1xD(i) <= nI4xD0(i);
      N0_2xD(i) <= nI0xD1(i);           -- third stage inputs
      N1_2xD(i) <= nI1xD1(i);
      N2_2xD(i) <= nI2xD1(i);
      N3_2xD(i) <= nI3xD1(i);
      N4_2xD(i) <= nI4xD1(i);
      N0_3xD(i) <= nI0xD2(i);           -- fourth stage inputs
      N1_3xD(i) <= nI1xD2(i);
      N2_3xD(i) <= nI2xD2(i);
      N3_3xD(i) <= nI3xD2(i);
      N4_3xD(i) <= nI4xD2(i);
      N0_4xD(i) <= nI0xD3(i);           -- fifth stage inputs
      N1_4xD(i) <= nI1xD3(i);
      N2_4xD(i) <= nI2xD3(i);
      N3_4xD(i) <= nI3xD3(i);
      N4_4xD(i) <= nI4xD3(i);
      N0_5xD(i) <= nI0xD4(i);           -- sixth stage inputs
      N1_5xD(i) <= nI1xD4(i);
      N2_5xD(i) <= nI2xD4(i);
      N3_5xD(i) <= nI3xD4(i);
      N4_5xD(i) <= nI4xD4(i);

      -- Uses only 5 pipeline registers for LR AND
      if NUM_PIPELINE_REGS(1) = 4 then
        I0_1xD(i) <= I0(i);             -- second stage inputs
        I1_1xD(i) <= I1(i);
        I2_1xD(i) <= I2(i);
        I3_1xD(i) <= I3(i);
        I4_1xD(i) <= I4(i);
        -- inverted inputs
        N0_1xD(i) <= nI0(i);            -- second stage inputs
        N1_1xD(i) <= nI1(i);
        N2_1xD(i) <= nI2(i);
        N3_1xD(i) <= nI3(i);
        N4_1xD(i) <= nI4(i);
      elsif NUM_PIPELINE_REGS(1) = 3 then
        I0_2xD(i) <= I0(i);             -- third stage inputs
        I1_2xD(i) <= I1(i);
        I2_2xD(i) <= I2(i);
        I3_2xD(i) <= I3(i);
        I4_2xD(i) <= I4(i);
        -- inverted inputs
        N0_2xD(i) <= nI0(i);            -- third stage inputs
        N1_2xD(i) <= nI1(i);
        N2_2xD(i) <= nI2(i);
        N3_2xD(i) <= nI3(i);
        N4_2xD(i) <= nI4(i);
      elsif NUM_PIPELINE_REGS(1) = 2 then
        I0_3xD(i) <= I0(i);             -- fourth stage inputs
        I1_3xD(i) <= I1(i);
        I2_3xD(i) <= I2(i);
        I3_3xD(i) <= I3(i);
        I4_3xD(i) <= I4(i);
        -- inverted inputs
        N0_3xD(i) <= nI0(i);            -- fourth stage inputs
        N1_3xD(i) <= nI1(i);
        N2_3xD(i) <= nI2(i);
        N3_3xD(i) <= nI3(i);
        N4_3xD(i) <= nI4(i);
      elsif NUM_PIPELINE_REGS(1) = 1 then
        I0_4xD(i) <= I0(i);             -- fifth stage inputs
        I1_4xD(i) <= I1(i);
        I2_4xD(i) <= I2(i);
        I3_4xD(i) <= I3(i);
        I4_4xD(i) <= I4(i);
        -- inverted inputs
        N0_4xD(i) <= nI0(i);            -- fifth stage inputs
        N1_4xD(i) <= nI1(i);
        N2_4xD(i) <= nI2(i);
        N3_4xD(i) <= nI3(i);
        N4_4xD(i) <= nI4(i);
      elsif NUM_PIPELINE_REGS(1) = 0 then
        I0_5xD(i) <= I0(i);             -- sixth stage inputs
        I1_5xD(i) <= I1(i);
        I2_5xD(i) <= I2(i);
        I3_5xD(i) <= I3(i);
        I4_5xD(i) <= I4(i);
        -- inverted inputs
        N0_5xD(i) <= nI0(i);            -- sixth stage inputs
        N1_5xD(i) <= nI1(i);
        N2_5xD(i) <= nI2(i);
        N3_5xD(i) <= nI3(i);
        N4_5xD(i) <= nI4(i);
      end if;
    end loop;
  end process inputs_for_low_randomness_ands_p;
  
  -----------------------------------------------------------------------------
  -- DOM AND gates
  -----------------------------------------------------------------------------
  dom_variant_g : if SBOX_VARIANT = "DOM" generate
    not_I0_and_I1 : entity work.dom_and
      generic map (
        PIPELINED => PIPELINED,
        D         => D)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => nI0,
        YxDI   => I1,
        ZxDI   => Z0xDI,
        QxDO   => A0);

    not_I1_and_I2 : entity work.dom_and
      generic map (
        PIPELINED => PIPELINED,
        D         => D)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => nI1,
        YxDI   => I2,
        ZxDI   => Z1xDI,
        QxDO   => A1);

    not_I2_and_I3 : entity work.dom_and
      generic map (
        PIPELINED => PIPELINED,
        D         => D)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => nI2,
        YxDI   => I3,
        ZxDI   => Z2xDI,
        QxDO   => A2);

    not_I3_and_I4 : entity work.dom_and
      generic map (
        PIPELINED => PIPELINED,
        D         => D)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => nI3,
        YxDI   => I4,
        ZxDI   => Z3xDI,
        QxDO   => A3);

    not_I4_and_I0 : entity work.dom_and
      generic map (
        PIPELINED => PIPELINED,
        D         => D)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => nI4,
        YxDI   => I0,
        ZxDI   => Z4xDI,
        QxDO   => A4);     
  end generate dom_variant_g;

  -----------------------------------------------------------------------------
  -- Low Ranomness AND gates
  -----------------------------------------------------------------------------
  low_rand_variant_g : if SBOX_VARIANT = "LOW_RADOMNESS" generate

    not_I0_and_I1: entity work.low_randomness_and
      generic map (
        D                       => D,
        USE_BELAID_OPTIMIZATION => 1)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => N0_0xD,   
        X0xD   => N0_1xD, 
        X1xD   => N0_2xD,
        X2xD   => N0_3xD,
        X3xD   => N0_4xD,
        X4xD   => N0_5xD,
        YxDI   => I1_0xD,
        Y0xD   => I1_1xD,
        Y1xD   => I1_2xD,
        Y2xD   => I1_3xD,
        Y3xD   => I1_4xD,
        Y4xD   => I1_5xD,
        ZxDI   => Z0xDI,
        QxDO   => A0);

    not_I1_and_I2 : entity work.low_randomness_and
      generic map (
        D                       => D,
        USE_BELAID_OPTIMIZATION => 1)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => N1_0xD,   
        X0xD   => N1_1xD, 
        X1xD   => N1_2xD,
        X2xD   => N1_3xD,
        X3xD   => N1_4xD,
        X4xD   => N1_5xD,
        YxDI   => I2_0xD,
        Y0xD   => I2_1xD,
        Y1xD   => I2_2xD,
        Y2xD   => I2_3xD,
        Y3xD   => I2_4xD,
        Y4xD   => I2_5xD,
        ZxDI   => Z1xDI,
        QxDO   => A1);

    not_I2_and_I3 : entity work.low_randomness_and
      generic map (
        D                       => D,
        USE_BELAID_OPTIMIZATION => 1)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => N2_0xD,   
        X0xD   => N2_1xD, 
        X1xD   => N2_2xD,
        X2xD   => N2_3xD,
        X3xD   => N2_4xD,
        X4xD   => N2_5xD,
        YxDI   => I3_0xD,
        Y0xD   => I3_1xD,
        Y1xD   => I3_2xD,
        Y2xD   => I3_3xD,
        Y3xD   => I3_4xD,
        Y4xD   => I3_5xD,
        ZxDI   => Z2xDI,
        QxDO   => A2);

    not_I3_and_I4 : entity work.low_randomness_and
      generic map (
        D                       => D,
        USE_BELAID_OPTIMIZATION => 1)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => N3_0xD,   
        X0xD   => N3_1xD, 
        X1xD   => N3_2xD,
        X2xD   => N3_3xD,
        X3xD   => N3_4xD,
        X4xD   => N3_5xD,
        YxDI   => I4_0xD,
        Y0xD   => I4_1xD,
        Y1xD   => I4_2xD,
        Y2xD   => I4_3xD,
        Y3xD   => I4_4xD,
        Y4xD   => I4_5xD,
        ZxDI   => Z3xDI,
        QxDO   => A3);

    not_I4_and_I0 : entity work.low_randomness_and
      generic map (
        D                       => D,
        USE_BELAID_OPTIMIZATION => 1)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => N4_0xD,   
        X0xD   => N4_1xD, 
        X1xD   => N4_2xD,
        X2xD   => N4_3xD,
        X3xD   => N4_4xD,
        X4xD   => N4_5xD,
        YxDI   => I0_0xD,
        Y0xD   => I0_1xD,
        Y1xD   => I0_2xD,
        Y2xD   => I0_3xD,
        Y3xD   => I0_4xD,
        Y4xD   => I0_5xD,
        ZxDI   => Z4xDI,
        QxDO   => A4);   
  end generate low_rand_variant_g;
  
end architecture beh;

-------------------------------------------------------------------------------
