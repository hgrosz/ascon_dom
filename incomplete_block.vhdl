-------------------------------------------------------------------------------
-- Title      : "incomplete block" 
-- Project    : 
-------------------------------------------------------------------------------
-- File       : incomplete_block.vhdl
-- Author     :   <hgross@T440HG>
-- Company    : 
-- Created    : 2016-12-14
-- Last update: 2017-01-24
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: part of generic shared AND gate with less randomness
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-14  1.0      hgross	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ascon_pkg.all;

-------------------------------------------------------------------------------

entity incomplete_block is

  generic (
    D                       : integer := 1;  -- protection order
    BLOCK_INDEX             : integer := 0   -- number of the complete block for
                                             -- calculating correct offsets
    );

  port (
    ClkxCI : in  std_logic;
    RstxBI : in  std_logic;
    -- Shares of X and Y
    X3xDP : in  t_shared_bit(D downto 0);
    Y3xDP : in  t_shared_bit(D downto 0);
    -- Fresh masks
    ZxDI  : in  t_shared_bit(((D+1)/2)-1 downto 0);
    -- Output Q = X*Y (masked)
    QxDO  : out t_shared_bit(D downto 0)
    );

end entity incomplete_block;

-------------------------------------------------------------------------------

architecture str of incomplete_block is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  -- Five register stages required
  signal P0xDN, P0xDP : t_shared_bit(D downto 0);
begin  -- architecture str

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  -- purpose: Implements the "incomplete block" functionality
  combinatorial_logic_p : process (P0xDP, X3xDP,
                                   Y3xDP, ZxDI) is
    variable number_of_complete_blocks : integer;
  begin  -- process combinatorial_logic_p
    for i in D downto 0 loop
      P0xDN(i) <= P0xDP(i);             -- default
      

      -- Calculate last block index
      number_of_complete_blocks := D / 4;

      -- DOM variant
      -- 1. Calculate two Z + multiplication terms in parallel: eg (AxBy +Z0)
      P0xDN(i) <= ZxDI(i mod ((D + 1)/2)) xor (X3xDP(i) and Y3xDP((i + 2*number_of_complete_blocks + 1) mod (D + 1)));

      -- output
      QxDO(i)  <= P0xDP(i);
    end loop;  -- D
  end process combinatorial_logic_p;

  -- purpose: Register process
  register_p: process (ClkxCI, RstxBI) is
  begin  -- process register_p
    for i in D downto 0 loop
      if RstxBI = '0' then              -- asynchronous reset (active low)
        P0xDP(i) <= '0';
      elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
        P0xDP(i) <= P0xDN(i);
      end if;
    end loop;  -- D
  end process register_p;

end architecture str;

-------------------------------------------------------------------------------
