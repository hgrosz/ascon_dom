-------------------------------------------------------------------------------
-- Title      : "complete block" 
-- Project    : 
-------------------------------------------------------------------------------
-- File       : complete_block.vhdl
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

entity complete_block is

  generic (
    D              : integer := 4;      -- protection order
    BLOCK_INDEX    : integer := 0       -- number of the complete block for
                                        -- calculating correct offsets
    );

  port (
    ClkxCI : in  std_logic;
    RstxBI : in  std_logic;
    -- Shares of X and Y
    XxDI  : in t_shared_bit(D downto 0);
    YxDI  : in t_shared_bit(D downto 0);
    -- Pipelined versions of inputs:
    X0xDP : in t_shared_bit(D downto 0);
    X1xDP : in t_shared_bit(D downto 0);
    X2xDP : in t_shared_bit(D downto 0);
    X3xDP : in t_shared_bit(D downto 0);
    Y0xDP : in t_shared_bit(D downto 0);
    Y1xDP : in t_shared_bit(D downto 0);
    Y2xDP : in t_shared_bit(D downto 0);
    Y3xDP : in t_shared_bit(D downto 0);
    -- Fresh masks
    ZxDI  : in t_shared_bit(D downto 0);
    -- Output Q = X*Y (masked)
    QxDO   : out t_shared_bit(D downto 0)
    );

end entity complete_block;

-------------------------------------------------------------------------------

architecture str of complete_block is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  -- Five register stages required
  signal P0xDN, P0xDP : t_shared_bit(D downto 0);
  signal P1xDN, P1xDP : t_shared_bit(D downto 0);
  signal P2xDN, P2xDP : t_shared_bit(D downto 0);
  signal P3xDN, P3xDP : t_shared_bit(D downto 0);
  signal P4xDN, P4xDP : t_shared_bit(D downto 0);

  -- Z requires two pipeline stages
  signal Z0xDP        : t_shared_bit(D downto 0);
  signal Z1xDP        : t_shared_bit(D downto 0);
begin  -- architecture str

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  -- purpose: Implements the "complete block" functionality
  combinatorial_logic_p: process (P0xDP, P1xDP, P2xDP, P3xDP, P4xDP,
                                  X0xDP, X2xDP, X3xDP, XxDI,
                                  Y0xDP, Y2xDP, Y3xDP, YxDI,
                                  Z1xDP, ZxDI) is
  begin  -- process combinatorial_logic_p
    for i in D downto 0 loop
      P0xDN(i) <= P0xDP(i); -- default
      P1xDN(i) <= P1xDP(i);
      P2xDN(i) <= P2xDP(i);
      P3xDN(i) <= P3xDP(i);
      P4xDN(i) <= P4xDP(i);

      -- 1. add Z shares to first multiplication term: Z + X*ROT(Y, 2*b + 1)
      P0xDN(i) <= ZxDI(i) xor (XxDI(i) and YxDI((i + 2*BLOCK_INDEX + 1) mod (D + 1)));
      -- 2. add mirrored term + .. ROT(X, 2*b +1)*ROT(Y, 2*b +1)
      P1xDN(i) <= P0xDP(i) xor (X0xDP((i + 2*BLOCK_INDEX + 1) mod (D + 1)) and Y0xDP(i));
      -- 3. add rotated Z share: + ROT(Z,1)
      P2xDN(i) <= P1xDP(i) xor Z1xDP((i+1) mod (D + 1));
      -- 4. add next multiplication term: + X*ROT(Y, 2*b + 2)
      P3xDN(i) <= P2xDP(i) xor (X2xDP(i) and Y2xDP((i + 2*BLOCK_INDEX + 2) mod (D + 1)));
      -- 5. add mirrored term: + ROT(X, 2*b + 2)*Y
      P4xDN(i) <= P3xDP(i) xor (X3xDP((i + 2*BLOCK_INDEX + 2) mod (D + 1)) and Y3xDP(i));

      -- output
      QxDO(i) <= P4xDP(i);
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
        P3xDP(i) <= '0';
        P4xDP(i) <= '0';
        Z0xDP(i) <= '0';
        Z1xDP(i) <= '0';
      elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
        P0xDP(i) <= P0xDN(i);
        P1xDP(i) <= P1xDN(i);
        P2xDP(i) <= P2xDN(i);
        P3xDP(i) <= P3xDN(i);
        P4xDP(i) <= P4xDN(i);
        Z0xDP(i) <= ZxDI(i);
        Z1xDP(i) <= Z0xDP(i);
      end if;
    end loop;  -- D
  end process register_p;

end architecture str;

-------------------------------------------------------------------------------
