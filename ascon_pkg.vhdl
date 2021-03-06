-------------------------------------------------------------------------------
-- Title      : Types and functions
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ascon_pkg.vhdl
-- Author     : Hannes Gross
-- Company    : Graz University of Technology
-- Created    : 2016-11-17
-- Last update: 2017-01-24
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Types and functions required for implementation
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-17  1.0      hgross  Created
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package ascon_pkg is

  -- Configuration
  type ascon_t is (Ascon128, Ascon128a);
  constant VARIANT         : ascon_t := Ascon128;  -- Ascon128 or Ascon128a
  constant SBOX_VARIANT    : string  := "LOW_RADOMNESS";  -- "DOM" or "LOW_RADOMNESS"
  constant D               : natural := 1;     -- protection order
  constant PARALLEL_SBOXES : integer := 1;     -- number of parallel
                                               -- S/boxes (1-64)
  constant IMPLICIT_AFFINE : string  := "no";  -- Sbox affine transform. shall be
                                        -- done during round const addition
                                        -- and shifting?  only makes sense when
					-- 64 parallel sboxes are used!

  -- Functions
  function NUM_PIPELINE_REGS (USE_BELAID_OPTIMIZATION : integer) return natural;  -- how deep is the main pipeline in the Sbox?
  function NUM_MASKS return natural;    -- return number of masks required
  -- Calculate number of required masks for selected variant of AND with
  -- reduced randomness cost
  function NUM_MASKS_REDUCED (USE_BELAID_OPTIMIZATION : integer) return integer;
  function DATA_SIZE return natural;    -- return the size of one data block
  function YESORNO_TO_0OR1 (PARAM                     : string) return integer;  -- convert 0 or 1 to "yes"
                                        -- or "no" 

  -- Rotate a 64-bit word
  function ROTATE_STATE_WORD (
    word            : std_logic_vector(63 downto 0);
    constant rotate : integer)
    return std_logic_vector;

  -- Create a 0 bit vector of arbitrary length
  function ZEROS (
    constant WIDTH : natural)
    return std_logic_vector;

  -- Types
  type t_shared_bit is array (natural range <>) of std_logic;
  type t_shared_data is array (natural range <>) of std_logic_vector(DATA_SIZE-1 downto 0);
  type t_shared_state_var is array (natural range <>) of std_logic_vector(63 downto 0);
  type t_shared_key_var is array (natural range <>) of std_logic_vector(127 downto 0);
  type t_shared_tag is array (natural range <>) of std_logic_vector(127 downto 0);
  type sbox_data_t is array (natural range <>) of t_shared_bit(D downto 0);
  type random_t is array (natural range <>) of t_shared_bit(NUM_MASKS -1 downto 0);

  -- Ascons FSM states
  type ascon_fsm_t is (IDLE, INIT, ASSOCIATED_DATA, PTCT_DATA, FINALIZE, READ_TAG);

end ascon_pkg;

package body ascon_pkg is

  -- Functions
  function NUM_PIPELINE_REGS (USE_BELAID_OPTIMIZATION : integer) return natural is
  begin
    if D = 0 then
      return 0;
    end if;

    if SBOX_VARIANT = "LOW_RADOMNESS" then
      -- LR SBOX variant
      if D = 1 then
        return 1;
      elsif D = 2 then
        if USE_BELAID_OPTIMIZATION = 1 then
          return 3;
        else
          return 2;
        end if;
      elsif D = 3 then
        return 4;
      else
        return 5;
      end if;
    else
      -- DOM
      return 1;
    end if;
  end function;

  function NUM_MASKS return natural is
  begin
    if SBOX_VARIANT = "LOW_RADOMNESS" then
      return NUM_MASKS_REDUCED (1);
    else
      if D = 0 then
        return 1;
      else
        return D*(D+1)/2;
      end if;
    end if;
  end function;

  function NUM_MASKS_REDUCED (USE_BELAID_OPTIMIZATION : integer) return integer is
    variable num_masks : integer := 0;
  begin
    num_masks := (D+1) * (D/4);         -- for complete blocks
    -- pseudo complete?
    if (D mod 4) = 3 then
      num_masks := num_masks + (D+1);
    -- half complete
    elsif ((D mod 4) = 2) then
      -- Optimization applicable?
      if (USE_BELAID_OPTIMIZATION = 1) and (D = 2) then
        num_masks := num_masks + (2*(D+1))/3;
      else
        num_masks := num_masks + (D+1);
      end if;
    -- incomplete
    elsif ((D mod 4) = 1) then
      -- Optimization applicable?
      --if (USE_BELAID_OPTIMIZATION = 1) and (((D+1) mod 3) = 0) then
      --num_masks := num_masks + (2*(D+1))/3;
      --else
      num_masks := num_masks + (D+1)/2;
    --end if;
    end if;
    return num_masks;
  end function;

  function DATA_SIZE return natural is
  begin
    if VARIANT = Ascon128 then
      return 64;
    else
      return 128;
    end if;
  end function;

  function ROTATE_STATE_WORD (
    word            : std_logic_vector(63 downto 0);
    constant rotate : integer)
    return std_logic_vector is
    variable x : std_logic_vector(63 downto 0);
  begin  -- ROTATE_STATE_WORD
    x := word(ROTATE-1 downto 0) & word(63 downto ROTATE);
    return x;
  end ROTATE_STATE_WORD;

  function ZEROS (
    constant WIDTH : natural)
    return std_logic_vector is
    variable x : std_logic_vector(WIDTH-1 downto 0);
  begin  -- ZEROS
    x := (others => '0');
    return x;
  end ZEROS;

  function YESORNO_TO_0OR1 (
    PARAM : string)
    return integer is
  begin
    if PARAM = "no" then
      return 0;
    elsif PARAM = "yes" then
      return 1;
    else
      return -1;
    end if;
  end YESORNO_TO_0OR1;

end ascon_pkg;
