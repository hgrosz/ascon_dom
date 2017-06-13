-------------------------------------------------------------------------------
-- Title      : DOM AND
-- Project    : 
-------------------------------------------------------------------------------
-- File       : dom_and.vhdl
-- Author     : Hannes Gross
-- Company    : Graz University of Technology
-- Created    : 2016-11-17
-- Last update: 2017-01-24
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: DOM masked AND gate
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

entity dom_and is

  generic (
    PIPELINED  : string  := "no";
    D          : integer := 2   -- protection order d = 0 ... 
    );

  port (
    -- Clock and reset
    ClkxCI : in  std_logic;
    RstxBI : in  std_logic;
    -- Shares of X and Y
    XxDI   : in  t_shared_bit(D downto 0);
    YxDI   : in  t_shared_bit(D downto 0);
    -- Fresh masks
    ZxDI   : in  t_shared_bit(NUM_MASKS -1 downto 0);
    -- Output Q = X*Y (masked)
    QxDO   : out t_shared_bit(D downto 0)
    );

end entity dom_and;

-------------------------------------------------------------------------------

architecture str of dom_and is
  
  -- Intermediates
  signal Xi_mul_Yj : t_shared_bit((D+1)*(D+1)-1 downto 0);
  
  -- Synchronization FF's
  signal FFxDN     : t_shared_bit((D+1)*(D+1)-1 downto 0);
  signal FFxDP     : t_shared_bit((D+1)*(D+1)-1 downto 0);
  
begin  -- architecture str

  -------------------------------------------------------------------
  -- General stuff:
  -- Generate multiplication terms
  mult_terms_p: process (XxDI, YxDI) is
  begin  -- process mult_terms_p
    for i in D downto 0 loop
      for j in D downto 0 loop
        Xi_mul_Yj((D+1)*i + j) <= XxDI(i) and YxDI(j);
      end loop;  -- j
    end loop;  -- i
  end process mult_terms_p;

  -- purpose: Register process
  register_proc_seq : process (ClkxCI, RstxBI) is
  begin  -- process register_proc_seq
    if RstxBI = '0' then                -- asynchronous reset (active low)
      for i in D downto 0 loop
        for j in D downto 0 loop
          FFxDP((D+1)*i + j) <= '0';
        end loop; --j
      end loop; -- i
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      for i in D downto 0 loop
        for j in D downto 0 loop
          FFxDP((D+1)*i + j) <= FFxDN((D+1)*i + j);
        end loop; --j
      end loop; -- i
    end if;
  end process register_proc_seq;

  ------------------------------------------------------------------
  -- Masked AND pipelined
  pipelined_g : if (PIPELINED = "yes") generate
    -- purpose: implements the shared multiplication in a secure and generic way
    shared_mul_p : process (FFxDP, Xi_mul_Yj, ZxDI) is
      variable result : std_logic;
    begin  -- process odd_shared_mul_p
      -- iterate over shares
      for i in 0 to D loop
        result := '0';
        for j in 0 to D loop
          -- Fi = Xi*Yi + SUM(...
          if (i = j) then
            FFxDN((D+1)*i + j) <= Xi_mul_Yj((D+1)*i + j);             -- domain term
          elsif (j > i) then
            FFxDN((D+1)*i + j) <= Xi_mul_Yj((D+1)*i + j) xor ZxDI(i + j*(j-1)/2);  -- regular term
          else
            FFxDN((D+1)*i + j) <= Xi_mul_Yj((D+1)*i + j) xor ZxDI(j + i*(i-1)/2);  -- transposed
          end if;
          -- Output
          result := result xor FFxDP((D+1)*i + j);
        end loop;  -- j     
        QxDO(i) <= result;
      end loop;  -- i
    end process shared_mul_p;
  end generate pipelined_g;

  -------------------------------------------------------------------
  -- Masked AND not pipelined
  not_pipelined_g : if (PIPELINED = "no") generate
    -- purpose: implements the shared multiplication in a secure and generic way
    -- type   : combinational
    -- inputs : 
    -- outputs: 
    shared_mul_p : process (FFxDN, FFxDP, Xi_mul_Yj, ZxDI) is
      variable result : std_logic;
    begin  -- process odd_shared_mul_p
      -- iterate over shares
      for i in 0 to D loop
        result := '0';
        for j in 0 to D loop
          -- Fi = Xi*Yi + SUM(...
          if (i = j) then
            FFxDN((D+1)*i + j) <= Xi_mul_Yj((D+1)*i + j);             -- domain term
          elsif (j > i) then
            FFxDN((D+1)*i + j) <= Xi_mul_Yj((D+1)*i + j) xor ZxDI(i + j*(j-1)/2);  -- regular term
          else
            FFxDN((D+1)*i + j) <= Xi_mul_Yj((D+1)*i + j) xor ZxDI(j + i*(i-1)/2);  -- transposed
          end if;
          -- Output
          if (i = j) then
            result := result xor FFxDN((D+1)*i + j);
          else
            result := result xor FFxDP((D+1)*i + j);
          end if;
        end loop;  -- j
        QxDO(i) <= result;
      end loop;  -- i
    end process shared_mul_p;
  end generate not_pipelined_g;
  
end architecture str;

-------------------------------------------------------------------------------
