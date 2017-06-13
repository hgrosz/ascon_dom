-------------------------------------------------------------------------------
-- Title      : "half complete block" 
-- Project    : 
-------------------------------------------------------------------------------
-- File       : half_complete_block.vhdl
-- Author     :   <hgross@T440HG>
-- Company    : 
-- Created    : 2016-12-14
-- Last update: 2017-01-23
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

entity half_complete_block is

  generic (
    D                       : integer := 2;  -- protection order
    USE_BELAID_OPTIMIZATION : integer := 1;  -- 0... just DOM
    BLOCK_INDEX             : integer := 0   -- number of the complete block for
                                             -- calculating correct offsets
    );

  port (
    ClkxCI : in  std_logic;
    RstxBI : in  std_logic;
    -- Pipelined versions of X and Y
    X2xDP : in t_shared_bit(D downto 0);
    X3xDP : in t_shared_bit(D downto 0);
    Y2xDP : in t_shared_bit(D downto 0);
    Y3xDP : in t_shared_bit(D downto 0);
    -- Fresh masks
    ZxDI   : in  t_shared_bit(D downto 0);
    -- Output Q = X*Y (masked)
    QxDO   : out t_shared_bit(D downto 0)
    );

end entity half_complete_block;

-------------------------------------------------------------------------------

architecture str of half_complete_block is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  -- Three register stages at maximum required
  signal O0xDN, O0xDP : t_shared_bit(D downto 0);
  signal P0xDN, P0xDP : t_shared_bit(D downto 0);
  signal P1xDN, P1xDP : t_shared_bit(D downto 0);
  signal P2xDN, P2xDP : t_shared_bit(D downto 0);
begin  -- architecture str

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  -- purpose: Implements the "half complete block" functionality
  combinatorial_logic_p: process (O0xDP, P0xDP, P1xDP, P2xDP,
                                  X2xDP, Y2xDP, X3xDP, Y3xDP,
                                  ZxDI) is
  variable number_of_complete_blocks : integer;
  begin  -- process combinatorial_logic_p
    for i in D downto 0 loop
      P0xDN(i) <= P0xDP(i); -- default
      P1xDN(i) <= P1xDP(i);
      P2xDN(i) <= P2xDP(i);
      O0xDN(i) <= O0xDP(i);
      QxDO(i)  <= P2xDP(i);  -- when pipelining
      
      -- Calculate last block index
      number_of_complete_blocks := D / 4;

      -- Belaid variant, only when applicable
      if USE_BELAID_OPTIMIZATION = 1 and ((D+1) mod 3) = 0 then
        -- ASSUMPTION: only lower 2/3 of Z shares are fresh random numbers, rest is 0
        -- 1. Calculate correct Z sharing
        if (i < 2*(D+1)/3) then
          P0xDN(i) <= ZxDI(i); -- do not modify 2/3 of Z shares
        else -- Calculate Z in upper third as combination of two Zs
          P0xDN(i) <= ZxDI(i - 2*(D+1)/3) xor  ZxDI(i - (D+1)/3);
        end if;
             
        -- 2. Add first term
        P1xDN(i) <= P0xDP(i) xor (X2xDP(i) and Y2xDP( (i + 2*number_of_complete_blocks + 1) mod (D + 1)));
        -- 3. Add mirrored terms
        P2xDN(i) <= P1xDP(i) xor (X3xDP( (i + 2*number_of_complete_blocks + 1) mod (D + 1)) and Y3xDP(i));
   
      else -- DOM variant
        -- 1. Calculate two Z + multiplication terms in parallel: eg (AxBy +Z0)
        P0xDN(i) <= ZxDI(i) xor (X3xDP(i) and Y3xDP( (i + 2*number_of_complete_blocks + 1) mod (D + 1)));
        -- and (AxCy + Z2)
        O0xDN(i) <= ZxDI((i + 2*number_of_complete_blocks + 2) mod (D + 1)) xor (X3xDP(i) and Y3xDP( (i + 2*number_of_complete_blocks + 2) mod (D + 1)));
        -- 2. Add terms together
        QxDO(i) <= O0xDP(i) xor P0xDP(i);

      end if;
    end loop;  -- D
  end process combinatorial_logic_p;

  -- purpose: Register process
  register_p: process (ClkxCI, RstxBI) is
  begin  -- process register_p
    for i in D downto 0 loop
      if RstxBI = '0' then              -- asynchronous reset (active low)
        P0xDP(i) <= '0';
        P1xDP(i) <= '0';
        P2xDP(i) <= '0';
        O0xDP(i) <= '0';
      elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
        P0xDP(i) <= P0xDN(i);
        P1xDP(i) <= P1xDN(i);
        P2xDP(i) <= P2xDN(i);
        O0xDP(i) <= O0xDN(i);
      end if;
    end loop;  -- D
  end process register_p;

end architecture str;

-------------------------------------------------------------------------------
