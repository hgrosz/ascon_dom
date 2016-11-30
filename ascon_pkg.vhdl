-------------------------------------------------------------------------------
-- Title      : Types and functions
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ascon_pkg.vhdl
-- Author     : Hannes Gross
-- Company    : Graz University of Technology
-- Created    : 2016-11-17
-- Last update: 2016-11-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Types and functions required for implementation
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-17  1.0      hgross	Created
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package ascon_pkg is

  -- Configuration
  type ascon_t is (Ascon128, Ascon128a);
  constant VARIANT         : ascon_t := Ascon128;  -- Ascon128 or Ascon128a
  constant D               : natural := 1;         -- protection order
  constant PARALLEL_SBOXES : integer := 1;         -- number of parallel S/boxes
  constant IMPLICIT_AFFINE : string  := "no";      -- Sbox affine transform. shall be
                                                   -- done during round const addition
                                                   -- and shifting?  
  
  -- Functions
  function NUM_MASKS (D : natural) return natural; -- return number of masks required  
  function DATA_SIZE return natural;               -- return the size of one data block
  
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
  type t_shared_bit       is array (natural range <>) of std_logic;
  type t_shared_data      is array (natural range <>) of std_logic_vector(DATA_SIZE-1 downto 0);
  type t_shared_state_var is array (natural range <>) of std_logic_vector( 63 downto 0);
  type t_shared_key_var   is array (natural range <>) of std_logic_vector(127 downto 0);
  type t_shared_tag       is array (natural range <>) of std_logic_vector(127 downto 0);
  type sbox_data_t        is array (natural range <>) of t_shared_bit(D downto 0);
  type random_t           is array (natural range <>) of t_shared_bit(NUM_MASKS(D)-1 downto 0);
  
  -- Ascons FSM states
  type ascon_fsm_t is (IDLE, INIT, ASSOCIATED_DATA, PTCT_DATA, FINALIZE, READ_TAG);
  
end ascon_pkg;
        
package body ascon_pkg is
  
  -- Functions
  function NUM_MASKS (D : natural) return natural is
  begin
    if D=0 then
      return 1;
    end if;
    return D*(D+1)/2;
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
  
end ascon_pkg;
