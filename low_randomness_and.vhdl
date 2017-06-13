-------------------------------------------------------------------------------
-- Title      : Low randomness (dth order) AND gate 
-- Project    : 
-------------------------------------------------------------------------------
-- File       : low_randomness_and.vhdl
-- Author     :   <hgross@T440HG>
-- Company    : 
-- Created    : 2016-12-14
-- Last update: 2017-01-23
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Top module of generic shared AND gate with low randomness
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

entity low_randomness_and is

  generic (
    D                       : integer := 1;  -- protection order
    USE_BELAID_OPTIMIZATION : integer := 1   -- 0... just DOM
    );

  port (
    ClkxCI : in  std_logic;
    RstxBI : in  std_logic;
    -- Shares of X and Y
    XxDI   : in  t_shared_bit(D downto 0);
    YxDI   : in  t_shared_bit(D downto 0);
    -- Pipelined input shares (this is an area optimization)
    X0xD : in t_shared_bit(D downto 0);
    X1xD : in t_shared_bit(D downto 0);
    X2xD : in t_shared_bit(D downto 0);
    X3xD : in t_shared_bit(D downto 0);
    X4xD : in t_shared_bit(D downto 0);
    Y0xD : in t_shared_bit(D downto 0);
    Y1xD : in t_shared_bit(D downto 0);
    Y2xD : in t_shared_bit(D downto 0);
    Y3xD : in t_shared_bit(D downto 0);
    Y4xD : in t_shared_bit(D downto 0);
    -- Fresh masks
    ZxDI   : in  t_shared_bit(NUM_MASKS_REDUCED(USE_BELAID_OPTIMIZATION)-1 downto 0);
    -- Output Q = X*Y (masked)
    QxDO   : out t_shared_bit(D downto 0)
    );

end entity low_randomness_and;

-------------------------------------------------------------------------------

architecture str of low_randomness_and is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  constant NUM_COMPLETE_BLOCKS : integer := D/4;

  -- Collect signals from different blocks for result:
  type intermediate_result_t is array (natural range <>) of t_shared_bit(D downto 0);
  signal TxD : intermediate_result_t (NUM_COMPLETE_BLOCKS downto 0);

  -- Generated port inputs
  signal HalfCompleteDataInxD : t_shared_bit(D downto 0);
begin  -- architecture str

  -----------------------------------------------------------------------------
  -- Generate complete blocks
  -----------------------------------------------------------------------------
  create_complete_blocks_g : for block_i in NUM_COMPLETE_BLOCKS-1 downto 0 generate
    complete_block_i : entity work.complete_block
      generic map (
        D              => D,
        BLOCK_INDEX    => block_i)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        XxDI   => XxDI,
        YxDI   => YxDI,
        X0xDP  => X0xD,
        X1xDP  => X1xD,
        X2xDP  => X2xD,
        X3xDP  => X3xD,
        Y0xDP  => Y0xD,
        Y1xDP  => Y1xD,
        Y2xDP  => Y2xD,
        Y3xDP  => Y3xD,
        ZxDI   => ZxDI((D+1)*(block_i+1)-1 downto (D+1)*block_i),  -- select correct randomnes range here!
        QxDO   => TxD(block_i));
  end generate create_complete_blocks_g;

  -----------------------------------------------------------------------------
  -- Other block (last block if needed) 
  -----------------------------------------------------------------------------
  -- Pseudo complete block
  create_pseudo_block_g : if (D mod 4) = 3 generate
    pseudo_complete_block_1: entity work.pseudo_complete_block
      generic map (
        D              => D,
        BLOCK_INDEX    => NUM_COMPLETE_BLOCKS)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,      
        X0xDP  => X0xD,
        X1xDP  => X1xD,
        X3xDP  => X3xD,
        Y0xDP  => Y0xD,
        Y1xDP  => Y1xD,
        Y3xDP  => Y3xD,
        ZxDI   => ZxDI(ZxDI'HIGH  downto (D+1)*NUM_COMPLETE_BLOCKS),
        QxDO   => TxD(NUM_COMPLETE_BLOCKS));
  end generate create_pseudo_block_g;

  -- Half complete block
  create_half_complete_block_g : if (D mod 4) = 2 generate
    half_complete_block_1: entity work.half_complete_block
      generic map (
        D                       => D,
        USE_BELAID_OPTIMIZATION => USE_BELAID_OPTIMIZATION,
        BLOCK_INDEX             => NUM_COMPLETE_BLOCKS)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        X2xDP  => X2xD,
        X3xDP  => X3xD,
        Y2xDP  => Y2xD,
        Y3xDP  => Y3xD,
        ZxDI   => HalfCompleteDataInxD,--ZxDI(ZxDI'HIGH  downto (D+1)*NUM_COMPLETE_BLOCKS),
        QxDO   => TxD(NUM_COMPLETE_BLOCKS));
  end generate create_half_complete_block_g;

  half_complete_block_data_in_p: process (HalfCompleteDataInxD, ZxDI) is
  begin  -- process half_complete_block_data_in_p
    for i in D downto 0 loop     -- per Z share
      if USE_BELAID_OPTIMIZATION = 1 then
        if (i < 2*(D+1)/3) then  -- use Z inputs for lower 2/3
          HalfCompleteDataInxD(i) <= ZxDI(i);
        else -- keep space left for higher 1/3 which becomes the combination of
             -- two shares of higher 2/3
          HalfCompleteDataInxD(i) <= '0';
        end if;
      else
        HalfCompleteDataInxD(i) <= ZxDI(i);
      end if;
    end loop;
  end process half_complete_block_data_in_p;

  -- Incomplete block
  create_incomplete_block_g : if (D mod 4) = 1 generate
    incomplete_block_1 : entity work.incomplete_block
      generic map (
        D              => D,
        BLOCK_INDEX    => NUM_COMPLETE_BLOCKS)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        X3xDP  => X3xD,
        Y3xDP  => Y3xD,
        ZxDI   => ZxDI(ZxDI'high downto (D+1)*NUM_COMPLETE_BLOCKS),
        QxDO   => TxD(NUM_COMPLETE_BLOCKS));
  end generate create_incomplete_block_g;


  -----------------------------------------------------------------------------
  -- Calculate output
  -----------------------------------------------------------------------------
  calculate_result_p : process (TxD, XxDI, YxDI, X4xD, Y4xD) is
    variable int_result : t_shared_bit(D downto 0);
  begin  -- process calculate_result_p
    for i in D downto 0 loop                      -- per share

      -- with or without pipelining?
      int_result(i) := X4xD(i) and Y4xD(i); -- + domain term;
  

      -- add output share of complete block
      for j in NUM_COMPLETE_BLOCKS-1 downto 0 loop  
        int_result(i) := int_result(i) xor TxD(j)(i);
      end loop;  -- j

      -- other blocks used?
      if (D mod 4) /= 0 then
        int_result(i) := int_result(i) xor TxD(NUM_COMPLETE_BLOCKS)(i);
      end if;

      -- output
      QxDO(i) <= int_result(i);
    end loop;  -- i
  end process calculate_result_p;
end architecture str;

-------------------------------------------------------------------------------
